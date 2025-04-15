pub trait JsSerialize {
    fn serialize(&self) -> JsValue;
}

impl JsSerialize for bool {
    fn serialize(&self) -> JsValue {
        js_sys::Boolean::from(*self).into()
    }   
}

impl JsSerialize for Loc {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"offset".into(), &self.offset.into()).unwrap();
        Reflect::set(&obj, &"length".into(), &self.len.into()).unwrap();
        obj.into()
    }
}

impl<T: JsSerialize + Clone> JsSerialize for AstElement<T> {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"element".into(), &self.element.serialize()).unwrap();
        Reflect::set(&obj, &"loc".into(), &self.loc.serialize()).unwrap();
        obj.into()
    }
}

impl<T: JsSerialize> JsSerialize for Vec<T> {
    fn serialize(&self) -> JsValue {
        let arr = Array::new();
        for item in self {
            arr.push(&item.serialize());
        }
        arr.into()
    }
}

impl<T: JsSerialize> JsSerialize for Box<T> {
    fn serialize(&self) -> JsValue {
        self.as_ref().serialize()
    }
}

impl<T: JsSerialize> JsSerialize for Option<T> {
    fn serialize(&self) -> JsValue {
        match self {
            Some(value) => value.serialize(),
            None => JsValue::null(),
        }
    }
}

impl JsSerialize for Soi {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"Soi".into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for Eoi {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"Eoi".into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for AstTypeReference {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"TypeReference".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &self.0.as_str().into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for AstUppercaseReference {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"UppercaseReference".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &self.0.as_str().into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for AstValueReference {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"ValueReference".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &self.0.as_str().into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for AstStringLiteral {
    fn serialize(&self) -> JsValue {
        let item = {
            let obj = Object::new();
            Reflect::set(&obj, &"kind".into(), &self.kind.to_string().as_str().into()).unwrap();
            Reflect::set(&obj, &"data".into(), &self.data.as_str().into()).unwrap();
            obj.into()
        };

        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"StringLiteral".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &item).unwrap();
        obj.into()
    }
}

impl JsSerialize for AstNumber {
    fn serialize(&self) -> JsValue {
        let bigint = js_sys::BigInt::new(&self.0.to_string().as_str().into()).expect("AstNumber -> bigint");

        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"Number".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &bigint.into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for Keyword {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"Keyword".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &self.name().into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for Operator {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"Operator".into()).unwrap();
        Reflect::set(&obj, &"item".into(), &self.name().into()).unwrap();
        obj.into()
    }
}

impl JsSerialize for AstBracedTokenStream {
    fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(&obj, &"ast".into(), &"BracedTokenStream".into()).unwrap();
        obj.into()
    }
}
