mod utils;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use std::collections::{HashMap, HashSet};
use std::{fs::File, io::Write};
use syn::{parse_macro_input, parse_str, ItemMod};
use utils::proc_macro_meta::{FieldItem2, GetGrammarTypeExt, ToIdentExt};

#[proc_macro_attribute]
pub fn pesticide(_attr: TokenStream, input: TokenStream) -> TokenStream {
    //let grammar = parse_macro_input!(attr as LitStr).value();
    let enable_builtin_ws = true;
    let root_mod = parse_macro_input!(input as ItemMod);
    // let mod_vis = root_mod.vis.to_token_stream().to_string();
    let mod_name = root_mod.ident.to_string();

    let mut builtin_types = HashSet::new();
    let mut structs = HashMap::new();
    let mut enums = HashMap::new();

    for (_, root_items) in root_mod.content.iter() {
        for root_item in root_items {
            if let syn::Item::Struct(root_struct) = root_item {
                let struct_name = root_struct.ident.to_string();
                let mut struct_fields = vec![];
                for root_field in root_struct.fields.iter() {
                    let (name_grammar, ctx_grammar) = root_field.attrs.get_grammar_type();
                    struct_fields.push(FieldItem2 {
                        name: root_field.ident.as_ref().unwrap().to_string(),
                        native_type: root_field.ty.to_token_stream().to_string().replace(" ", ""),
                        name_grammar,
                        ctx_grammar,
                    });
                }
                structs.insert(struct_name, struct_fields);
            } else if let syn::Item::Enum(root_enum) = root_item {
                let enum_name = root_enum.ident.to_string();
                let mut enum_fields = vec![];
                for root_field in root_enum.variants.iter() {
                    let (name_grammar, ctx_grammar) = root_field.attrs.get_grammar_type();
                    enum_fields.push(FieldItem2 {
                        name: root_field.ident.to_string(),
                        native_type: root_field
                            .fields
                            .iter()
                            .next()
                            .map(|p| p.to_token_stream().to_string().replace(" ", ""))
                            .unwrap_or("()".to_string()),
                        name_grammar,
                        ctx_grammar,
                    });
                }
                enums.insert(enum_name, enum_fields);
            }
        }
    }

    // process field item grammars
    let mut def_types: HashSet<_> = structs.keys().map(|s| s.to_string()).collect();
    def_types.extend(enums.keys().map(|e| e.to_string()));
    let structs: HashMap<_, _> = structs
        .into_iter()
        .map(|(struct_name, fields)| {
            let fields: Vec<_> = fields
                .into_iter()
                .map(|field| {
                    field.to_field_item(&mut def_types, &mut builtin_types, &mod_name, &struct_name)
                })
                .collect();
            (struct_name, fields)
        })
        .collect();
    let enums: HashMap<_, _> = enums
        .into_iter()
        .map(|(enum_name, fields)| {
            let fields: Vec<_> = fields
                .into_iter()
                .map(|field| {
                    field.to_field_item(&mut def_types, &mut builtin_types, &mod_name, &enum_name)
                })
                .collect();
            (enum_name, fields)
        })
        .collect();

    let mut pest_cnt = "".to_string();
    pest_cnt.push_str(
        r#"// This file is generated by crate: pesticide
// Please do not modify this file

WS = _{ " " | "\t" | NEWLINE }
COMMENT    = _{ ("//" ~ (!NEWLINE ~ ANY)*) | ("/*" ~ (!"*/" ~ ANY)* ~ "*/") }



"#,
    );
    for builtin_type in builtin_types.into_iter() {
        let builtin_grammar = match builtin_type {
            "bool_literal" => r#" "true" | "false" "#,
            "num_literal" => {
                r#" ("-"?~ASCII_DIGIT+) | (("0x"|"0X")~HEX_DIGIT+ ) | (("0b"|"0B")~("0"|"1")+) "#
            }
            "float_literal" => {
                r#""nan" | "-"?~"inf" | ("-"?~ASCII_DIGIT+ ~("."~ASCII_DIGIT*)) | ("-"?~ASCII_DIGIT+ ~("."~ASCII_DIGIT+)) "#
            }
            "string_literal" => r#" "\"" ~ ("\\\"" | (!"\"" ~ ANY))* ~ "\"" "#,
            _ => panic!("unknown builtin type: {}", builtin_type),
        };
        pest_cnt.push_str(&format!("{} = @{{ {} }}\n", builtin_type, builtin_grammar));
    }
    pest_cnt.push_str("\n");

    for (struct_name, struct_fields) in structs.iter() {
        pest_cnt.push_str(&format!("// struct - {}::{}\n", mod_name, struct_name));
        for field in struct_fields.iter() {
            pest_cnt.push_str(&format!(
                "{}_{}_{} = {}\n",
                mod_name,
                struct_name,
                field.name,
                field.ctx_grammar.serilize()
            ));
        }
        let struct_fields = struct_fields
            .iter()
            .map(|field| field.get_grammar_item(&mod_name, struct_name, enable_builtin_ws))
            .collect::<Vec<_>>();
        pest_cnt.push_str(&match enable_builtin_ws {
            true => format!(
                "{}_{} = {{ WS* ~ {} ~ WS* }}\n",
                mod_name,
                struct_name,
                struct_fields.join(" ~ WS* ~ ")
            ),
            false => format!(
                "{}_{} = {{ {} }}\n",
                mod_name,
                struct_name,
                struct_fields.join(" ~ ")
            ),
        });
        pest_cnt.push_str(&format!(
            "entry_{0}_{1} = {{ SOI ~ {0}_{1} ~ EOI }}\n\n",
            mod_name, struct_name
        ));
    }
    for (enum_name, enum_fields) in enums.iter() {
        pest_cnt.push_str(&format!("// enum - {}::{}\n", mod_name, enum_name));
        for field in enum_fields.iter() {
            pest_cnt.push_str(&format!(
                "{}_{}_{} = {}\n",
                mod_name,
                enum_name,
                field.name,
                field.ctx_grammar.serilize()
            ));
        }
        pest_cnt.push_str(&format!(
            "{}_{} = {{ {} }}\n",
            mod_name,
            enum_name,
            enum_fields
                .iter()
                .map(|field| field.get_grammar_item(&mod_name, enum_name, enable_builtin_ws))
                .collect::<Vec<_>>()
                .join(" | ")
        ));
        let struct_fields = enum_fields
            .iter()
            .map(|field| field.get_grammar_item(&mod_name, enum_name, enable_builtin_ws))
            .collect::<Vec<_>>();
        pest_cnt.push_str(&match enable_builtin_ws {
            true => format!(
                "{}_{} = {{ WS* ~ ({}) ~ WS* }}\n",
                mod_name,
                enum_name,
                struct_fields.join(" | ")
            ),
            false => format!(
                "{}_{} = {{ {} }}\n",
                mod_name,
                enum_name,
                struct_fields.join(" | ")
            ),
        });
        pest_cnt.push_str(&format!(
            "entry_{0}_{1} = {{ SOI ~ {0}_{1} ~ EOI }}\n\n",
            mod_name, enum_name
        ));
    }
    File::create("target/pesticide.pest")
        .unwrap()
        .write_all(pest_cnt.as_bytes())
        .unwrap();

    let structs: Vec<_> = structs
        .into_iter()
        .map(|(struct_name, fields)| {
            let root_fields: Vec<_> = fields
                .iter()
                .map(|field| {
                    let name = field.name.to_ident();

                    let native_type: syn::Type = parse_str(&field.native_type).unwrap();
                    quote! { pub #name: #native_type, }
                })
                .collect();
            let field_init: Vec<_> = fields
                .iter()
                .map(|field| {
                    let name = field.name.to_ident();
                    let init_expr: syn::Expr = parse_str(&field.get_struct_init()).unwrap();
                    quote! { #name: #init_expr }
                })
                .collect();
            let field_parse: Vec<_> = fields
                .iter()
                .map(|field| {
                    let name_full =
                        format!("{}_{}_{}", mod_name, struct_name, field.name).to_ident();
                    let parse_expr: syn::Expr =
                        parse_str(&field.get_struct_parse(&def_types)).unwrap();
                    quote! { Rule::#name_full => #parse_expr, }
                })
                .collect();
            let entry_name = format!("{}_{}", mod_name, struct_name).to_ident();
            let entry_name2 = format!("entry_{}_{}", mod_name, struct_name).to_ident();
            let struct_name = struct_name.to_ident();
            quote! {
                #[derive(Debug)]
                pub struct #struct_name {
                    #(#root_fields)*
                }

                impl #struct_name {
                    pub fn parse_impl(root: pest::iterators::Pair<Rule>) -> anyhow::Result<Self> {
                        let mut ret = Self {
                            #(#field_init),*
                        };
                        for root_item in root.into_inner() {
                            match root_item.as_rule() {
                                Rule::#entry_name => return Self::parse_impl(root_item),
                                Rule::#entry_name2 => return Self::parse_impl(root_item),
                                #(#field_parse)*
                                _ => unreachable!(),
                            }
                        }
                        Ok(ret)
                    }

                    pub fn try_parse(data: &str) -> anyhow::Result<Self> {
                        let root = PesticideParser::parse(Rule::#entry_name, data)?
                            .next()
                            .ok_or(anyhow::Error::msg("no context found"))?;
                        Self::parse_impl(root)
                    }
                }
            }
        })
        .collect();

    let enums: Vec<_> = enums
        .into_iter()
        .map(|(enum_name, fields)| {
            let root_fields: Vec<_> = fields
                .iter()
                .map(|field| {
                    let name = field.name.to_ident();
                    if field.native_type != "()" {
                        let native_type = field.native_type.to_ident();
                        quote! { #name(#native_type), }
                    } else {
                        quote! { #name, }
                    }
                })
                .collect();
            let mut field_parse = vec![];
            for field in fields.iter() {
                let name_full = format!("{}_{}_{}", mod_name, enum_name, field.name).to_ident();
                let (parse_expr, parse_expr1) = field.get_enum_parse(&def_types);
                let parse_expr: syn::Expr = parse_str(&parse_expr).unwrap();
                field_parse.push(quote! { Rule::#name_full => #parse_expr, });
                if let Some(parse_expr1) = parse_expr1 {
                    let parse_expr1: syn::Expr = parse_str(&parse_expr1).unwrap();
                    let name_full1 =
                        format!("{}_{}_{}_wrap", mod_name, enum_name, field.name).to_ident();
                    field_parse.push(quote! { Rule::#name_full1 => #parse_expr1, });
                }
            }
            let entry_name = format!("{}_{}", mod_name, enum_name).to_ident();
            let entry_name2 = format!("entry_{}_{}", mod_name, enum_name).to_ident();
            let enum_name = enum_name.to_ident();
            quote! {
                #[derive(Debug)]
                pub enum #enum_name {
                    #(#root_fields)*
                }

                impl #enum_name {
                    pub fn parse_impl(root: pest::iterators::Pair<Rule>) -> anyhow::Result<Self> {
                        let root_item = root.into_inner().next().unwrap();
                        match root_item.as_rule() {
                            Rule::#entry_name => Self::parse_impl(root_item),
                            Rule::#entry_name2 => Self::parse_impl(root_item),
                            #(#field_parse)*
                            _ => unreachable!(),
                        }
                    }

                    pub fn try_parse(data: &str) -> anyhow::Result<Self> {
                        let root = PesticideParser::parse(Rule::#entry_name, data)?
                            .next()
                            .ok_or(anyhow::Error::msg("no context found"))?;
                        Self::parse_impl(root)
                    }
                }
            }
        })
        .collect();

    let mod_name = mod_name.to_ident();
    let a = quote! {
        pub mod #mod_name {
            use pest::Parser;
            use pest_derive::Parser;

            #[derive(Parser)]
            #[grammar = "target/pesticide.pest"]
            pub struct PesticideParser;

            #(#structs)*
            #(#enums)*
        }
    };
    //panic!("AAAAAAA\n{}", a.to_token_stream().to_string());
    a.into()
}

// #[proc_macro_attribute]
// pub fn my_function(attr: TokenStream, item: TokenStream) -> TokenStream {
//     let function_name = attr.to_string();
//     let mut result = item.to_string();
//     result.push_str(&format!("fn {}() {{", function_name));
//     result.push_str("println!(\"This is a custom function generated by attribute macro!\"); }");
//     result.parse().unwrap()
// }
