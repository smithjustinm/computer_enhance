use clap::{Parser, Subcommand};
use computer_enhance::instruction::{Instruction, Mode, OperandType};
use computer_enhance::{MEMORY, REGISTERS_W0, REGISTERS_W1};
use std::fs::File;
use std::io::{Read, Write};

#[derive(Parser, Debug)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Decode {
        #[arg(short = 'f')]
        file: String,
    },
    Exec {
        #[arg(short = 'f')]
        file: String,
        /// Flag for dumping memory to data file
        #[arg(short, long)]
        dump: bool,
    },
}

fn look_up_reg(reg: usize, w: bool) -> String {
    if w {
        REGISTERS_W1[reg].to_string()
    } else {
        REGISTERS_W0[reg].to_string()
    }
}

fn look_up_jump(code: u8) -> Option<&'static str> {
    match code {
        0b01110100 => Some("je"),
        0b01111100 => Some("jl"),
        0b01111110 => Some("jle"),
        0b01110010 => Some("jb"),
        0b01110110 => Some("jbe"),
        0b01111010 => Some("jp"),
        0b01110000 => Some("jo"),
        0b01111000 => Some("js"),
        0b01110101 => Some("jne"),
        0b01111101 => Some("jnl"),
        0b01111111 => Some("jnle"),
        0b01110011 => Some("jnb"),
        0b01110111 => Some("jnbe"),
        0b01111011 => Some("jnp"),
        0b01110001 => Some("jno"),
        0b01111001 => Some("jns"),
        0b11100010 => Some("loop"),
        0b11100001 => Some("loope"),
        0b11100000 => Some("loopne"),
        0b11100011 => Some("jcxz"),
        _ => None,
    }
}

fn parse_instruction(values: &[u8], index: &mut usize) -> Option<Instruction> {
    let value = values[*index];

    if (value >> 4) & 0b1111 == 0b1011 {
        parse_immediate_to_register(values, index)
    } else if (value >> 1) & 0b1111111 == 0b0000010 {
        parse_to_accumulator(values, index, "add")
    } else if (value >> 1) & 0b1111111 == 0b00010110 {
        parse_to_accumulator(values, index, "sub")
    } else if (value >> 1) & 0b1111111 == 0b0011110 {
        parse_to_accumulator(values, index, "cmp")
    } else if look_up_jump(value & 0b11111111) != None {
        parse_jump(values, index, look_up_jump(value & 0b11111111).unwrap())
    } else if (value >> 1) & 0b1111111 == 0b1100011 {
        parse_immediate_to_memory(values, index, "mov")
    } else {
        parse_general_instruction(values, index)
    }
}

fn parse_immediate_to_register(values: &[u8], index: &mut usize) -> Option<Instruction> {
    // println!("immediate to register");
    let value = values[*index];
    let w = (value >> 3) & 0b1 != 0;
    let reg = (value & 0b111) as usize;
    let (data, negative_data) = if w {
        *index += 3;
        let raw_data = u16::from_le_bytes([values[*index - 2], values[*index - 1]]);
        let is_negative = raw_data & 0b1000_0000_0000_0000 != 0;
        let negative_data = if is_negative {
            Some(((65535 - raw_data + 1) as i32) * -1)
        } else {
            None
        };

        (raw_data, negative_data)
    } else {
        *index += 2;
        let raw_data = values[*index - 1] as u16;
        let is_negative = raw_data & 0b1000_0000 != 0;
        let negative_data = if is_negative {
            Some(((256 - raw_data) as i32) * -1)
        } else {
            None
        };
        (raw_data, negative_data)
    };

    Some(Instruction {
        opcode: "mov",
        oprand_type: OperandType::ImmReg,
        reg: Some(look_up_reg(reg, w)),
        rm: None,
        d: None,
        data: Some(data),
        negative_data,
        mode: None,
    })
}

fn parse_to_accumulator(
    values: &[u8],
    index: &mut usize,
    opcode: &'static str,
) -> Option<Instruction> {
    // println!("to accumulator");
    // println!("{:?}", values);
    let value = values[*index];
    let w = value & 0b1 != 0;
    let (data, negative_data) = if w {
        let data = u16::from_le_bytes([values[*index + 1], values[*index + 2]]);
        *index += 3;
        (data, None)
    } else {
        let signed_data = values[*index + 1] as u16;
        let is_negative = values[*index + 1] & 0b1000_0000 != 0;
        let data = if is_negative {
            Some(((256 - signed_data) as i32) * -1)
        } else {
            None
        };
        *index += 2;
        (signed_data, data)
    };

    Some(Instruction {
        opcode,
        oprand_type: OperandType::ImmReg,
        reg: Some(look_up_reg(0, w)),
        rm: None,
        d: None,
        data: Some(data),
        negative_data,
        mode: None,
    })
}

fn parse_jump(values: &[u8], index: &mut usize, opcode: &'static str) -> Option<Instruction> {
    let data = values[*index + 1] as u16;
    let is_negative = values[*index + 1] & 0b1000_0000 != 0;
    let negative_data = if is_negative {
        Some(((256 - data) as i32) * -1)
    } else {
        None
    };
    *index += 2;
    Some(Instruction {
        opcode,
        oprand_type: OperandType::Jump,
        reg: None,
        rm: None,
        d: None,
        data: Some(data),
        negative_data,
        mode: None,
    })
}

fn parse_immediate_to_memory(
    values: &[u8],
    index: &mut usize,
    opcode: &'static str,
) -> Option<Instruction> {
    // println!("immediate to memory");
    // println!("{:?}", values);
    let value = values[*index];
    let w = value & 0b1 != 0;
    let rm = (values[*index + 1] & 0b111) as usize;
    let mode = (values[*index + 1] >> 6) & 0b11;

    let (displacement, mode) = match mode {
        0b00 => {
            println!("{:?}", values);
            if rm == 6 {
                let dis = u16::from_le_bytes([values[*index + 2], values[*index + 3]]);
                *index += 4;
                (dis, Mode::Mode00)
            } else {
                panic!("Not supported");
            }
        }
        0b01 => {
            let dis = values[*index + 2] as u16;
            *index += 3;
            (dis, Mode::Mode01)
        }
        0b10 => {
            let dis = u16::from_le_bytes([values[*index + 2], values[*index + 3]]);
            *index += 4;
            (dis, Mode::Mode10)
        }
        _ => panic!("Not supported"),
    };
    let rm_value = format!("[{} + {}]", MEMORY[rm], displacement);
    let data = if w {
        *index += 2;
        u16::from_le_bytes([values[*index - 2], values[*index - 1]])
    } else {
        *index += 1;
        values[*index - 1] as u16
    };
    Some(Instruction {
        opcode,
        oprand_type: OperandType::ImmRm,
        reg: None,
        rm: Some(rm_value),
        d: None,
        data: Some(data),
        negative_data: None,
        mode: Some(mode),
    })
}

fn parse_general_instruction(values: &[u8], index: &mut usize) -> Option<Instruction> {
    // println!("general instruction");
    // println!("{:?}", values);
    // println!("{}", *index);
    let value = values[*index];
    // println!("{:06b}", (value >> 2) & 0b111111);
    let opcode = match (value >> 2) & 0b111111 {
        0b000000 => "add",
        0b100000 => "imm_arith",
        0b001010 => "sub",
        0b001110 => "cmp",
        0b100010 => "mov",
        _ => return None,
    };

    let w = value & 0b1 != 0;
    let d = (value >> 1) & 0b1 != 0;
    let mod_field = (values[*index + 1] >> 6) & 0b11;
    let reg = ((values[*index + 1] >> 3) & 0b111) as usize;
    let rm = (values[*index + 1] & 0b111) as usize;

    // println!("{:02b}", mod_field);

    match mod_field {
        0b00 => parse_mod_00(values, index, opcode, w, d, reg, rm),
        0b01 => parse_mod_01(values, index, opcode, w, d, reg, rm),
        0b10 => parse_mod_10(values, index, opcode, w, d, reg, rm),
        0b11 => parse_mod_11(values, index, opcode, w, d, reg, rm),
        _ => None,
    }
}

// Memory mode no displacement unless r/m is 110 then there will be 16 bit displacement
fn parse_mod_00(
    values: &[u8],
    index: &mut usize,
    opcode: &'static str,
    w: bool,
    d: bool,
    reg: usize,
    rm: usize,
) -> Option<Instruction> {
    // println!("mod_00);
    // println!("{:?}", values);
    let (oprand_type, opcode, reg_value, rm_value, data) = if opcode == "imm_arith" {
        let (opcode, data) = parse_imm_arith(values, index, w, 0);
        (
            OperandType::ImmRm,
            opcode,
            None,
            Some(format!("[{}]", MEMORY[rm])),
            Some(data),
        )
    } else if rm == 6 {
        let displacement = u16::from_le_bytes([values[*index + 2], values[*index + 3]]);
        *index += 4;
        let rm_value = if displacement == 0 {
            format!("[{}]", MEMORY[rm])
        } else {
            format!("[{} + {}]", MEMORY[rm], displacement)
        };
        (
            OperandType::RegMem,
            opcode,
            Some(look_up_reg(reg, w)),
            Some(rm_value),
            None,
        )
    } else {
        *index += 2;
        (
            OperandType::RegMem,
            opcode,
            Some(look_up_reg(reg, w)),
            Some(format!("[{}]", MEMORY[rm])),
            None,
        )
    };

    Some(Instruction {
        opcode,
        oprand_type,
        reg: reg_value,
        rm: rm_value,
        d: Some(d),
        data,
        negative_data: None,
        mode: Some(Mode::Mode00),
    })
}

// Memory mode with 8bit displacement
fn parse_mod_01(
    values: &[u8],
    index: &mut usize,
    opcode: &'static str,
    w: bool,
    d: bool,
    reg: usize,
    rm: usize,
) -> Option<Instruction> {
    // println!("mod_01");
    let displacement = values[*index + 2] as u16;
    let rm_value = if displacement == 0 {
        format!("[{}]", MEMORY[rm])
    } else {
        format!("[{} + {}]", MEMORY[rm], displacement)
    };
    *index += 3;

    Some(Instruction {
        opcode,
        oprand_type: OperandType::RegMem,
        reg: Some(look_up_reg(reg, w)),
        rm: Some(rm_value),
        d: Some(d),
        data: None,
        negative_data: None,
        mode: Some(Mode::Mode01),
    })
}

// Memory mode with 16bit displacement
fn parse_mod_10(
    values: &[u8],
    index: &mut usize,
    opcode: &'static str,
    w: bool,
    d: bool,
    reg: usize,
    rm: usize,
) -> Option<Instruction> {
    // println!("mod_10");
    // println!("{:?}", values);
    let displacement = u16::from_le_bytes([values[*index + 2], values[*index + 3]]);
    let (oprand_type, opcode, reg_value, data) = if opcode == "imm_arith" {
        let (opcode, data) = parse_imm_arith(values, index, w, 2);
        (OperandType::ImmRm, opcode, None, Some(data))
    } else {
        *index += 4;
        (OperandType::RegMem, opcode, Some(look_up_reg(reg, w)), None)
    };

    let rm_value = if displacement == 0 {
        format!("[{}]", MEMORY[rm])
    } else {
        format!("[{} + {}]", MEMORY[rm], displacement)
    };

    Some(Instruction {
        opcode,
        oprand_type,
        reg: reg_value,
        rm: Some(rm_value),
        d: Some(d),
        data,
        negative_data: None,
        mode: Some(Mode::Mode10),
    })
}

// Register mode no displacement
fn parse_mod_11(
    values: &[u8],
    index: &mut usize,
    opcode: &'static str,
    w: bool,
    d: bool,
    reg: usize,
    rm: usize,
) -> Option<Instruction> {
    // println!("mod_11");
    let (oprand_type, opcode, reg, data) = if opcode == "imm_arith" {
        let original_index = *index;
        let (opcode, data) = parse_imm_arith(values, index, w, 0);
        (
            OperandType::ImmReg,
            opcode,
            (values[original_index + 1] & 0b111) as usize,
            Some(data),
        )
    } else {
        *index += 2;
        (OperandType::RegMem, opcode, reg, None)
    };

    Some(Instruction {
        opcode,
        oprand_type,
        reg: Some(look_up_reg(reg, w)),
        rm: Some(look_up_reg(rm, w)),
        d: Some(d),
        data,
        negative_data: None,
        mode: Some(Mode::Mode11),
    })
}

fn parse_imm_arith(
    values: &[u8],
    index: &mut usize,
    w: bool,
    displacement: i32,
) -> (&'static str, u16) {
    let opcode = match (values[*index + 1] >> 3) & 0b111 {
        0b000 => "add",
        0b101 => "sub",
        0b111 => "cmp",
        _ => panic!("Invalid opcode"),
    };

    let s = (values[*index] >> 1) & 0b1 != 0;
    let data = if w && !s {
        let data = u16::from_le_bytes([
            values[*index + 2 + displacement as usize],
            values[*index + 3 + displacement as usize],
        ]);
        *index += 4 + displacement as usize;
        data
    } else {
        let data = values[*index + 2 + displacement as usize] as u16;
        *index += 3 + displacement as usize;
        data
    };

    (opcode, data)
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Decode { file } => {
            let mut file = File::open(file).expect("file not found");
            let mut values: Vec<u8> = Vec::new();
            file.read_to_end(&mut values).expect("Failed to read file");

            println!("bits 16\n");

            let mut index = 0;
            while index < values.len() {
                if let Some(instruction) = parse_instruction(&values, &mut index) {
                    println!("{}", instruction);
                } else {
                    index += 1;
                }
            }
        }
        Commands::Exec { file, dump } => {
            let mut file = File::open(file).expect("file not found");
            let mut values: Vec<u8> = Vec::new();
            file.read_to_end(&mut values).expect("Failed to read file");

            println!("bits 16\n");

            let mut index = 0;
            let mut prev_index = 0;
            let mut registers: Vec<i32> = vec![0; 8];
            let mut flag_register: Vec<u8> = vec![0; 2];
            let mut memory: Vec<u8> = vec![0; 65535];
            let mut clocks = 0;
            while index < values.len() {
                if let Some(instruction) = parse_instruction(&values, &mut index) {
                    instruction.execute(
                        &mut registers,
                        &mut flag_register,
                        &mut index,
                        &mut prev_index,
                        &mut memory,
                        &mut clocks,
                    );
                } else {
                    index += 1;
                }

                prev_index = index;
            }
            println!("Final registers:");
            registers.iter().enumerate().for_each(|(i, v)| {
                println!("{}: 0x{:04x} ({})", REGISTERS_W1[i], v, v);
            });
            println!("ip: 0x{:04x} ({})", index, index);
            let mut flags = "Flags: ".to_string();
            if flag_register[0] == 1 {
                flags.push('Z');
            }

            if flag_register[1] == 1 {
                flags.push('S');
            }
            println!("{}", flags);

            if dump {
                let mut file = File::create("memory.data").expect("Failed to create file");
                file.write_all(&memory).expect("Failed to write data");
            }
        }
    }
}
