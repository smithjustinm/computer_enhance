pub const REGISTERS_W0: [&str; 8] = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
pub const REGISTERS_W1: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
pub const MEMORY: [&str; 8] = [
    "bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx",
];

pub mod instruction;
