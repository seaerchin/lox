pub struct Scanner<'a> {
    source: &'a str,
}

type Tokens<'a> = &'a str;

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source }
    }

    pub fn scan_tokens(&self) -> Vec<Tokens> {
        self.source.split_ascii_whitespace().collect()
    }
}
