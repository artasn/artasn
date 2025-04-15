use artasn::compiler::parser::*;
use js_sys::{Array, Object, Reflect};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub unsafe fn code_tokenize(source: String) -> JsValue {
    let mut token_stream = TokenStream::from_string(&source, true);

    let tokens = Array::new();
    loop {
        match token_stream.try_next(None) {
            Ok(token) => match &token.kind {
                TokenKind::Soi => (),
                TokenKind::Eoi => break,
                kind => {
                    let obj = Object::new();
                    let (kind, info) = match (kind, &token.data) {
                        (TokenKind::ValueReference, _) => ("valuereference", None),
                        (TokenKind::TypeReference, _) => ("typereference", None),
                        (TokenKind::Keyword, Some(TokenData::Keyword(keyword))) => {
                            ("keyword", Some(keyword.name().into()))
                        }
                        (TokenKind::Number, _) => ("number", None),
                        (TokenKind::String, Some(TokenData::String(kind, _))) => {
                            ("string", Some(kind.to_string().into()))
                        }
                        (TokenKind::Operator, _) => ("operator", None),
                        (other, data) => unreachable!("{other:?}, {data:#?}"),
                    };
                    Reflect::set(&obj, &"kind".into(), &kind.into()).unwrap();
                    if let Some(info) = info {
                        Reflect::set(&obj, &"info".into(), &info).unwrap();
                    }
                    Reflect::set(&obj, &"loc".into(), &token.loc.serialize()).unwrap();
                    tokens.push(&obj.into());
                }
            },
            Err(err) => {
                let obj = Object::new();
                Reflect::set(&obj, &"error".into(), &(&err.kind.message()).into()).unwrap();
                Reflect::set(&obj, &"loc".into(), &err.loc.serialize()).unwrap();
                return obj.into();
            }
        }
    }
    tokens.into()
}

#[wasm_bindgen]
pub unsafe fn code_parse(source: String) -> JsValue {
    let mut token_stream = TokenStream::from_string(&source, true);
    let result = AstProgram::parse(ParseContext::new(&mut token_stream));
    match result {
        ParseResult::Ok(program) => program.serialize(),
        ParseResult::Error(err) | ParseResult::Fail(err) => {
            let obj = Object::new();
            Reflect::set(&obj, &"error".into(), &(&err.kind.message()).into()).unwrap();
            Reflect::set(&obj, &"loc".into(), &err.loc.serialize()).unwrap();
            return obj.into();
        }
    }
}
