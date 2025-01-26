use proc_macro::TokenStream;
use quote::ToTokens;
use std::collections::HashMap;

#[proc_macro_attribute]
pub fn pesticide(attr: TokenStream, input: TokenStream) -> TokenStream {
    let target_path = {
        let mut target_path: String = "target/pesticide.pest".to_string();
        let attr_parser = syn::meta::parser(|meta| {
            if meta.path.is_ident("target_path") {
                let ttarget_path: syn::LitStr = meta.value()?.parse()?;
                target_path = format!("{}/pesticide.pest", ttarget_path.value());
                Ok(())
            } else {
                Err(meta.error(format!("unsupported arg: {:?}", meta.path.get_ident())))
            }
        });
        syn::parse_macro_input!(attr with attr_parser);
        target_path
    };
    let pest_items = {
        let mut items: HashMap<String, GrammarCtx> = HashMap::new();
        items.insert("WS".into(), GrammarCtx::Builtin);
        items.insert("COMMENT".into(), GrammarCtx::Builtin);
        items.insert("ID".into(), GrammarCtx::Builtin);
        items.insert("bool_literal".into(), GrammarCtx::Builtin);
        items.insert("num_literal".into(), GrammarCtx::Builtin);
        items.insert("float_literal".into(), GrammarCtx::Builtin);
        items.insert("string_literal".into(), GrammarCtx::Builtin);
        //
        let input2 = input.clone();
        let root_mod = syn::parse_macro_input!(input2 as syn::ItemMod);
        let mod_name = root_mod.ident.to_string();
        for (_, root_items) in root_mod.content.iter() {
            for root_item in root_items {
                if let syn::Item::Struct(root_struct) = root_item {
                    let struct_name = root_struct.ident.to_string();
                    let item_name = format!("{mod_name}_{struct_name}");
                    let mut sub_items = vec![];
                    for root_field in root_struct.fields.iter() {
                        let field_name = root_field.ident.as_ref().unwrap().to_string();
                        let item_sub_name = format!("{item_name}_{field_name}");
                        let native_type =
                            root_field.ty.to_token_stream().to_string().replace(" ", "");
                        if root_field.attrs.len() > 1 {
                            panic!("current not support multiple attr");
                        }
                        let ctx = root_field
                            .attrs
                            .get(0)
                            .get_grammar_type(item_sub_name.clone(), native_type);
                        sub_items.push(GrammarCtx::Id(item_sub_name.clone()));
                        items.insert(item_sub_name, ctx);
                    }
                    items.insert(item_name, GrammarCtx::All(sub_items));
                } else if let syn::Item::Enum(root_enum) = root_item {
                    let enum_name = root_enum.ident.to_string();
                    let item_name = format!("{mod_name}_{enum_name}");
                    let mut sub_items = vec![];
                    for root_field in root_enum.variants.iter() {
                        let field_name = root_field.ident.to_string();
                        let item_sub_name = format!("{item_name}_{field_name}");
                        let native_type = root_field
                            .fields
                            .iter()
                            .next()
                            .map(|p| p.to_token_stream().to_string().replace(" ", ""))
                            .unwrap_or("()".to_string());
                        if root_field.attrs.len() > 1 {
                            panic!("current not support multiple attr");
                        }
                        let ctx = root_field
                            .attrs
                            .get(0)
                            .get_grammar_type(item_sub_name.clone(), native_type);
                        sub_items.push(GrammarCtx::Id(item_sub_name.clone()));
                        items.insert(item_sub_name, ctx);
                    }
                    items.insert(item_name, GrammarCtx::All(sub_items));
                }
            }
        }
        items
    };
    //
    quote::quote! {}.into()
}

struct GrammarCtxRepeat {
    pub field: GrammarCtx,
    pub split: GrammarCtx,
    pub allow_empty: bool,
}

enum GrammarCtx {
    Builtin,
    Id(String),
    Str(String),
    Option(Box<GrammarCtx>),
    All(Vec<GrammarCtx>),
    Any(Vec<GrammarCtx>),
    Repeat(Box<GrammarCtxRepeat>),
    Silent(Box<GrammarCtx>),
    Atomic(Box<GrammarCtx>),
    Raw(String),
}

trait AttributeExt {
    fn get_name(&self) -> Option<String>;
}

impl AttributeExt for syn::Attribute {
    fn get_name(&self) -> Option<String> {
        self.path().get_ident().map(|i| i.to_string())
    }
}

impl AttributeExt for syn::meta::ParseNestedMeta<'_> {
    fn get_name(&self) -> Option<String> {
        self.path.get_ident().map(|i| i.to_string())
    }
}

trait CalcItemExt {
    fn get_grammar_type(&self, name: String, native_type: String) -> GrammarCtx;
}

impl CalcItemExt for Option<&syn::Attribute> {
    fn get_grammar_type(&self, name: String, native_type: String) -> GrammarCtx {
        panic!("BBBBBBBBBB {:?}", self.to_token_stream());
        match self {
            Some(attr) => {
                if let Some(name) = attr.get_name() {
                    if name == "any" {
                        //let mut items = vec![];
                        panic!("AAAAAAAAAA {:?}", attr.to_token_stream());
                    }
                }
                todo!()
            }
            None => todo!(),
        }
        GrammarCtx::Builtin
    }
}

/*
// 匹配运算符 -> ast_Oper = "+" | "-" | "*" | "/" | "%" | "**"
#[any("+", "-", "*", "/", "%", "**")]
pub enum Oper {}
*/
