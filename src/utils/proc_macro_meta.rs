use anyhow::anyhow;
use lazy_static::lazy_static;
use proc_macro::Span;
use quote::ToTokens;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use syn::{meta::ParseNestedMeta, punctuated::Punctuated};
use syn::{Attribute, Expr, Ident, LitBool, LitStr, Meta, MetaList, Token};

pub trait AttrToValueExt {
    fn attr_to_value(meta_args: &MetaList) -> anyhow::Result<Self>
    where
        Self: Sized;
}

impl AttrToValueExt for String {
    fn attr_to_value(meta_list: &MetaList) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        Ok(meta_list.parse_args::<LitStr>()?.value())
    }
}

impl AttrToValueExt for bool {
    fn attr_to_value(meta_list: &MetaList) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        Ok(meta_list.parse_args::<LitBool>()?.value())
    }
}

pub trait ExprToValueExt {
    fn expr_to_value(expr: &syn::Expr) -> anyhow::Result<Self>
    where
        Self: Sized;
}

impl ExprToValueExt for String {
    fn expr_to_value(expr: &syn::Expr) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        match expr {
            syn::Expr::Lit(lit) => match &lit.lit {
                syn::Lit::Str(lit_str) => Ok(lit_str.value()),
                syn::Lit::Char(lit_char) => Ok(lit_char.value().to_string()),
                _ => panic!("expr[{:?}] not compatible to str.", expr.to_token_stream()),
            },
            _ => Err(anyhow!(format!(
                "expr[{:?}] not compatible to str.",
                expr.to_token_stream()
            ))),
        }
    }
}

impl ExprToValueExt for bool {
    fn expr_to_value(expr: &syn::Expr) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        match expr {
            syn::Expr::Lit(syn::ExprLit {
                attrs: _,
                lit: syn::Lit::Bool(lit_bool),
            }) => Ok(lit_bool.value),
            _ => Err(anyhow!(format!(
                "expr[{:?}] not compatible to bool.",
                expr.to_token_stream()
            ))),
        }
    }
}

impl ExprToValueExt for Expr {
    fn expr_to_value(expr: &syn::Expr) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        Ok(expr.clone())
    }
}

pub trait AttributeExt {
    fn get_name(&self) -> Option<String>;
}

impl AttributeExt for Attribute {
    fn get_name(&self) -> Option<String> {
        self.path().get_ident().map(|i| i.to_string())
    }
}

impl AttributeExt for ParseNestedMeta<'_> {
    fn get_name(&self) -> Option<String> {
        self.path.get_ident().map(|i| i.to_string())
    }
}

pub trait AttributeExt2 {
    fn get_arg<T: AttrToValueExt>(&self) -> anyhow::Result<T>;
    fn get_args_item<T: ExprToValueExt>(&self, key: &str) -> anyhow::Result<T>;
}

impl AttributeExt2 for Attribute {
    fn get_arg<T: AttrToValueExt>(&self) -> anyhow::Result<T> {
        let meta_args = self.meta.require_list()?;
        T::attr_to_value(meta_args)
    }

    fn get_args_item<T: ExprToValueExt>(&self, key: &str) -> anyhow::Result<T> {
        // let mut ret = Err(anyhow!("not find arg_key[{key}] in Attribute"));
        // self.parse_nested_meta(|meta| {
        //     if let Some(name) = meta.get_name() {
        //         if &name == key {
        //             if let Ok(value) = meta.() {
        //                 ret = T::attr_to_value(&value);
        //             }
        //         }
        //     }
        //     Err(anyhow!("not find arg_key[{key}] in Attribute"))
        // });
        // ret
        let meta_args = self.meta.require_list()?;
        let args = meta_args.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
        for arg in args.iter() {
            let arg = arg.require_name_value()?;
            if let Some(ident) = arg.path.get_ident() {
                let ident = ident.to_string();
                if &ident == key {
                    return T::expr_to_value(&arg.value);
                }
            }
        }
        Err(anyhow!("not find arg_key[{key}] in Attribute"))
    }
}

lazy_static! {
    pub static ref ALL_BUILTIN_TYPES: HashMap<&'static str, &'static str> = vec![
        ("bool", "bool_literal"),
        ("i8", "num_literal"),
        ("i16", "num_literal"),
        ("i32", "num_literal"),
        ("i64", "num_literal"),
        ("isize", "num_literal"),
        ("u8", "num_literal"),
        ("u16", "num_literal"),
        ("u32", "num_literal"),
        ("u64", "num_literal"),
        ("usize", "num_literal"),
        ("f32", "float_literal"),
        ("f64", "float_literal"),
        ("String", "string_literal"),
    ]
    .into_iter()
    .collect();
}

#[derive(Clone, Debug)]
pub struct NameGrammar {
    char: Option<String>,
    split: Option<String>,
    last_split: Option<bool>,
}

#[derive(Clone, Debug)]
pub enum CtxGrammar {
    Normal(String),
    Atomic(String),
    Silent(String),
}

impl CtxGrammar {
    pub fn serilize(&self) -> String {
        match self {
            Self::Normal(content) => format!("{{ {} }}", content),
            Self::Atomic(content) => format!("@{{ {} }}", content),
            Self::Silent(content) => format!("_{{ {} }}", content),
        }
    }
}

#[derive(Clone)]
pub struct FieldItem {
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub native_type: String,
    pub name_grammar: NameGrammar,
    pub ctx_grammar: CtxGrammar,
    pub init_value: Expr,
    pub ignore: bool,
}

#[derive(Clone)]
pub struct FieldItem2 {
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub native_type: String,
    pub name_grammar: Option<NameGrammar>,
    pub ctx_grammar: Option<CtxGrammar>,
    pub init_value: Option<Expr>,
    pub ignore: bool,
}

impl FieldItem {
    pub fn get_struct_parse(&self, def_types: &HashSet<String>) -> String {
        let (owrap, base_type) = self.native_type.get_base_type();
        let base_type = base_type.native_type_to_parse(def_types);
        match owrap {
            Some("Option") => format!("ret.{} = Some({})", self.name, base_type),
            Some("Vec") => format!("ret.{}.push({})", self.name, base_type),
            None => format!("ret.{} = {}", self.name, base_type),
            Some(wrap) => panic!("unknown wrap type: {}", wrap),
        }
    }

    pub fn get_enum_parse(&self, def_types: &HashSet<String>) -> (String, Option<String>) {
        match &self.native_type == "()" {
            true => (format!("Ok(Self::{})", self.name), None),
            false => {
                let (owrap, base_type) = self.native_type.get_base_type();
                let base_type = base_type.native_type_to_parse(def_types);
                match owrap {
                    Some("Option") => unimplemented!(),
                    Some("Vec") => unimplemented!(),
                    None => (format!("Ok(Self::{}({}))", self.name, base_type), None),
                    Some(wrap) => panic!("unknown wrap type: {}", wrap),
                }
            }
        }
    }

    pub fn get_grammar_item(
        &self,
        mod_name: &str,
        parent_name: &str,
        enable_builtin_ws: bool,
    ) -> String {
        let ret_base = format!("{}_{}_{}", mod_name, parent_name, self.name);
        let split = match (&self.name_grammar.split, enable_builtin_ws) {
            (Some(split), true) => Some(format!("WS* ~ \"{split}\" ~ WS*")),
            (Some(split), false) => Some(format!("\"{split}\"")),
            (None, true) => Some("WS*".to_string()),
            (None, false) => None,
        };
        match (&self.name_grammar.char, split, self.name_grammar.last_split) {
            (Some(char), Some(split), Some(true)) => {
                format!("({ret_base} ~ {split}){char}")
            }
            (Some(char), None, _) => {
                format!("{ret_base}{char}")
            }
            (Some(char), Some(split), Some(false)) => {
                let ret = format!("{ret_base} ~ ({split} ~ {ret_base})*");
                match &char[..] {
                    "+" => ret,
                    _ => format!("({ret})?"),
                }
            }
            (Some(char), Some(split), None) => {
                let ret = format!("{ret_base} ~ ({split} ~ {ret_base})* ~ {split}?");
                match &char[..] {
                    "+" => ret,
                    _ => format!("({ret})?"),
                }
            }
            (None, _, _) => ret_base,
        }
    }
}

impl FieldItem2 {
    pub fn to_field_item(
        &self,
        structs: &HashMap<String, (Vec<FieldItem2>, Vec<Attribute>)>,
        enums: &HashMap<String, (Vec<FieldItem2>, Vec<Attribute>)>,
        mod_name: &str,
        parent_name: &str,
    ) -> FieldItem {
        let name_grammar = self.name_grammar.clone().unwrap_or_else(|| {
            let native_type = &self.native_type[..];
            if native_type.starts_with("Vec<") && native_type.ends_with(">") {
                NameGrammar {
                    char: Some("*".to_string()),
                    split: None,
                    last_split: None,
                }
            } else if native_type.starts_with("Option<") && native_type.ends_with(">") {
                NameGrammar {
                    char: Some("?".to_string()),
                    split: None,
                    last_split: None,
                }
            } else {
                NameGrammar {
                    char: None,
                    split: None,
                    last_split: None,
                }
            }
        });
        let ctx_grammar = self.ctx_grammar.clone().unwrap_or_else(|| {
            let mut native_type = &self.native_type[..];
            if native_type.starts_with("Vec<") && native_type.ends_with(">") {
                native_type = &native_type[4..native_type.len() - 1];
            } else if native_type.starts_with("Option<") && native_type.ends_with(">") {
                native_type = &native_type[7..native_type.len() - 1];
            } else if self.ignore {
                return CtxGrammar::Normal("".to_string());
            }
            match structs.contains_key(native_type) || enums.contains_key(native_type) {
                true => CtxGrammar::Normal(format!("{}_{}", mod_name, native_type)),
                false => match ALL_BUILTIN_TYPES.get(&native_type) {
                    Some(builtin_type) => CtxGrammar::Normal((*builtin_type).to_string()),
                    None => {
                        panic!(
                            "unknown type[{}] in struct/enum item[{}::{}::{}]",
                            native_type, mod_name, parent_name, self.name
                        );
                    }
                },
            }
        });
        let init_value = self.init_value.clone().unwrap_or_else(|| {
            syn::parse_str(&match &self.native_type[..] {
                "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => Cow::Borrowed("0"),
                "f32" | "f64" => Cow::Borrowed("0.0"),
                "bool" => Cow::Borrowed("false"),
                "String" => Cow::Borrowed("\"\".to_string()"),
                "()" => Cow::Borrowed("()"),
                native_type => {
                    if native_type.starts_with("Option<") {
                        Cow::Borrowed("None")
                    } else if native_type.starts_with("Vec<") {
                        Cow::Borrowed("vec![]")
                    } else if let Some(struct_item) = structs.get(native_type) {
                        Cow::Owned(struct_item.0.make_struct_init_value(native_type))
                    } else if let Some(enum_item) = enums.get(native_type) {
                        Cow::Owned(enum_item.0.make_enum_init_value(native_type))
                    } else {
                        panic!("unknown init value for type: {}", self.native_type)
                    }
                }
            })
            .unwrap()
        });
        FieldItem {
            attrs: self.attrs.clone(),
            name: self.name.clone(),
            native_type: self.native_type.clone(),
            name_grammar: name_grammar,
            ctx_grammar: ctx_grammar,
            init_value: init_value,
            ignore: self.ignore,
        }
    }
}

pub trait GetGrammarTypeExt {
    fn get_grammar_type(&self, name: String, native_type: String) -> FieldItem2;
}

impl GetGrammarTypeExt for Vec<Attribute> {
    fn get_grammar_type(&self, name: String, native_type: String) -> FieldItem2 {
        let mut attrs = vec![];
        let mut name_grammar = None;
        let mut ctx_grammar = None;
        let mut init_value = None;
        let mut ignore = false;
        for attr in self.iter() {
            if let Some(name) = attr.get_name() {
                if &name == "repeat" {
                    // #[repeat(char = '+', split = ',', last_split = true)]
                    name_grammar = Some(NameGrammar {
                        char: attr.get_args_item::<String>("char").ok(),
                        split: attr.get_args_item::<String>("split").ok(),
                        last_split: attr.get_args_item::<bool>("last_split").ok(),
                    });
                } else if &name == "ID" {
                    ctx_grammar = Some(CtxGrammar::Normal("ID".to_string()));
                } else if ["normal", "atomic", "silent"].contains(&(&name[..])) {
                    // #[normal(r#" "+" | "-" | "*" | "/" "#)]
                    if let Ok(grammar_value) = attr.get_arg() {
                        match &name[..] {
                            "normal" => ctx_grammar = Some(CtxGrammar::Normal(grammar_value)),
                            "atomic" => ctx_grammar = Some(CtxGrammar::Atomic(grammar_value)),
                            "silent" => ctx_grammar = Some(CtxGrammar::Silent(grammar_value)),
                            _ => continue,
                        };
                    }
                } else if &name == "ignore" {
                    ignore = true;
                    init_value = attr.get_args_item::<Expr>("init_value").ok()
                } else {
                    attrs.push(attr.clone());
                }
            }
        }
        FieldItem2 {
            attrs,
            name,
            native_type,
            name_grammar,
            ctx_grammar,
            init_value,
            ignore,
        }
    }
}

pub trait ToIdentExt {
    fn to_ident(&self) -> Ident;
}

impl ToIdentExt for String {
    fn to_ident(&self) -> Ident {
        Ident::new(&self, Span::call_site().into())
    }
}

pub trait NativeTypeToParseExt {
    fn native_type_to_parse(&self, def_types: &HashSet<String>) -> String;
    fn get_base_type(&self) -> (Option<&str>, &str);
}

impl NativeTypeToParseExt for &str {
    fn native_type_to_parse(&self, def_types: &HashSet<String>) -> String {
        match &self[..] {
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64" => {
                "root_item.as_str().parse()?".to_string()
            }
            "bool" => "root_item.as_str().parse()?".to_string(),
            "String" => "root_item.as_str().to_string()".to_string(),
            "()" => "()".to_string(),
            _ if def_types.contains(*self) => format!("{}::parse_impl(root_item)?", self),
            _ => panic!("unknown native_type: {}", self),
        }
    }

    fn get_base_type(&self) -> (Option<&str>, &str) {
        match self.find("<") {
            Some(p) => (Some(&self[0..p]), &self[p + 1..self.len() - 1]),
            None => (None, self),
        }
    }
}

impl NativeTypeToParseExt for String {
    fn native_type_to_parse(&self, def_types: &HashSet<String>) -> String {
        (&self[..]).native_type_to_parse(def_types)
    }
    fn get_base_type(&self) -> (Option<&str>, &str) {
        match self.find("<") {
            Some(p) => (Some(&self[0..p]), &self[p + 1..self.len() - 1]),
            None => (None, self),
        }
    }
}

pub trait MakeInitValueExt {
    fn make_struct_init_value(&self, parent_name: &str) -> String;
    fn make_enum_init_value(&self, parent_name: &str) -> String;
}

impl MakeInitValueExt for Vec<FieldItem2> {
    fn make_struct_init_value(&self, parent_name: &str) -> String {
        let mut ret = format!("{parent_name} {{ ");
        for (index, field) in self.iter().enumerate() {
            if index > 0 {
                ret.push_str(", ");
            }
            ret.push_str(&format!(
                "{}: {}",
                field.name,
                field.native_type.native_type_to_parse(&HashSet::new())
            ));
        }
        ret.push_str(" }");
        ret
    }

    fn make_enum_init_value(&self, parent_name: &str) -> String {
        for field in self.iter() {
            if field.native_type == "()" {
                return format!("{}::{}", parent_name, field.name);
            }
        }
        panic!("[enum]");
    }
}
