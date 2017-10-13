use std::collections::BTreeMap;

pub struct OneToOne<T, S> {
    a_to_b: BTreeMap<T, S>,
    b_to_a: BTreeMap<S, T>,
}

impl<T: Ord + Clone, S: Ord + Clone> OneToOne<T, S> {
    pub fn new() -> OneToOne<T, S> {
        OneToOne {
            a_to_b: BTreeMap::new(),
            b_to_a: BTreeMap::new(),
        }
    }
    pub fn insert(&mut self, a: T, b: S) {
        self.a_to_b.insert(a.clone(), b.clone());
        self.b_to_a.insert(b, a);
    }
    pub fn get_a(&self, b: &S) -> Option<&T> {
        self.b_to_a.get(b)
    }
    pub fn get_b(&self, a: &T) -> Option<&S> {
        self.a_to_b.get(a)
    }
    pub fn a(&self) -> &BTreeMap<T, S> { &self.a_to_b }
    pub fn b(&self) -> &BTreeMap<S, T> { &self.b_to_a }
}