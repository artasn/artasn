use std::cell::RefCell;

use super::AstParser;

#[derive(Debug, Clone)]
pub struct LazyParse<Input, Data: Clone, Output: Clone> {
    data: Option<Data>,
    loader: Option<fn(&AstParser<'_>, &Input, &Data) -> Output>,
    loaded_output: RefCell<Option<Output>>,
}

impl<Input, Data: Clone, Output: Clone> LazyParse<Input, Data, Output> {
    pub fn new(data: Data, loader: fn(&AstParser<'_>, &Input, &Data) -> Output) -> Self {
        Self {
            data: Some(data),
            loader: Some(loader),
            loaded_output: RefCell::new(None),
        }
    }

    pub fn precomputed(output: Output) -> Self {
        Self {
            data: None,
            loader: None,
            loaded_output: RefCell::new(Some(output)),
        }
    }

    pub fn parse(&self, parser: &AstParser<'_>, input: &Input) -> Output {
        {
            let borrow = self.loaded_output.borrow().clone();
            if let Some(loaded_output) = borrow { return loaded_output }
        }

        let output = (self.loader.unwrap())(parser, input, self.data.as_ref().unwrap());

        let mut borrow = self.loaded_output.borrow_mut();
        *borrow = Some(output.clone());

        output
    }
}
