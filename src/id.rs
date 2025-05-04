use rand::Rng;

static CHARACTERS: [char; 36] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
];

pub struct IdGenerator {
    length: usize,
}

impl IdGenerator {
    pub fn new(length: usize) -> Self {
        Self { length }
    }

    pub fn generate(&self) -> String {
        let mut rng = rand::rng();
        let mut id = String::new();
        for _ in 0..self.length {
            id.push(CHARACTERS[rng.random_range(0..CHARACTERS.len())]);
        }
        id
    }

    pub fn generate_unique(&self, existing_ids: &Vec<String>) -> String {
        let mut id = self.generate();
        while existing_ids.contains(&id) {
            id = self.generate();
        }
        id
    }
}