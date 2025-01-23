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
        for (_, root_items) in root_mod.content.iter() {
            for root_item in root_items {
                if let syn::Item::Struct(root_struct) = root_item {
                    let struct_name = root_struct.ident.to_string();
                    let item_name = format!("{struct_name}_{struct_name}");
                    let mut sub_items = vec![];
                    for root_field in root_struct.fields.iter() {
                        let name = root_field.ident.as_ref().unwrap().to_string();
                        let native_type =
                            root_field.ty.to_token_stream().to_string().replace(" ", "");
                        if root_field.attrs.len() > 1 {
                            panic!("current not support multiple attr");
                        }
                        let ctx: GrammarCtx = match root_field.attrs.len() == 0 {
                            true => todo!(),
                            false => todo!(),
                        };
                        let item_sub_name = format!("{item_name}_{name}");
                        sub_items.push(GrammarCtx::Id(item_sub_name));
                        // struct_fields.push(root_field.attrs.get_grammar_type(name, native_type));
                    }
                    items.insert(item_name, GrammarCtx::All(sub_items));
                } else if let syn::Item::Enum(root_enum) = root_item {
                    let enum_name = root_enum.ident.to_string();
                    // let mut enum_fields = vec![];
                    // for root_field in root_enum.variants.iter() {
                    //     let name = root_field.ident.to_string();
                    //     let native_type = root_field.fields.iter().next()
                    //         .map(|p| p.to_token_stream().to_string().replace(" ", ""))
                    //         .unwrap_or("()".to_string());
                    //     enum_fields.push(root_field.attrs.get_grammar_type(name, native_type));
                    // }
                    // enums.insert(enum_name, (enum_fields, root_enum.attrs.clone()));
                }
            }
        }
        items
    };
    //
    input
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

/*
// 匹配运算符 -> ast_Oper = "+" | "-" | "*" | "/" | "%" | "**"
#[any("+", "-", "*", "/", "%", "**")]
pub enum Oper {}
*/
