use crate::REGISTERS_W1;
use regex::Regex;
use std::fmt::Display;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: &'static str,
    pub oprand_type: OperandType,
    pub reg: Option<String>,
    pub rm: Option<String>,
    pub d: Option<bool>,
    pub data: Option<u16>,
    pub negative_data: Option<i32>,
    pub mode: Option<Mode>,
}

#[derive(Debug)]
pub enum OperandType {
    RegMem, // Register/Memory to/from Register
    ImmReg, //Immediate to Register
    ImmRm,  // Immediate to Register/Memory
    Jump,   // Jump
}

#[derive(Debug)]
pub enum Mode {
    Mode00,
    Mode01,
    Mode10,
    Mode11,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.oprand_type {
            OperandType::RegMem => {
                if self.d.unwrap() {
                    write!(
                        f,
                        "{} {}, {}",
                        self.opcode,
                        self.reg.as_ref().unwrap(),
                        self.rm.as_ref().unwrap()
                    )
                } else {
                    write!(
                        f,
                        "{} {}, {}",
                        self.opcode,
                        self.rm.as_ref().unwrap(),
                        self.reg.as_ref().unwrap()
                    )
                }
            }
            OperandType::ImmReg => match self.negative_data {
                Some(data) => write!(
                    f,
                    "{} {}, {}",
                    self.opcode,
                    self.reg.as_ref().unwrap(),
                    data
                ),
                None => write!(
                    f,
                    "{} {}, {}",
                    self.opcode,
                    self.reg.as_ref().unwrap(),
                    self.data.unwrap()
                ),
            },
            OperandType::ImmRm => match self.negative_data {
                Some(data) => write!(f, "{} {}, {}", self.opcode, self.rm.as_ref().unwrap(), data),
                None => write!(
                    f,
                    "{} {}, {}",
                    self.opcode,
                    self.rm.as_ref().unwrap(),
                    self.data.unwrap()
                ),
            },
            OperandType::Jump => match self.negative_data {
                Some(data) => write!(f, "{} {}", self.opcode, data),
                None => write!(f, "{} {}", self.opcode, self.data.unwrap()),
            },
        }
    }
}

impl Instruction {
    pub fn execute(
        &self,
        registers: &mut [i32],
        flag_registers: &mut [u8],
        ip: &mut usize,
        prev_ip: &mut usize,
        memory: &mut [u8],
        clocks: &mut usize,
    ) {
        match self.opcode {
            "mov" => match self.oprand_type {
                OperandType::ImmReg => {
                    let reg_text = self.reg.as_ref().unwrap();
                    let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                    let data = self.data.unwrap() as i32;
                    *clocks += 4;
                    println!(
                        "mov {}, {} ; Clocks: +4 = {} | {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x}",
                        reg_text, data, clocks, reg_text, registers[reg], data, *prev_ip, *ip,
                    );
                    registers[reg] = data
                }
                OperandType::RegMem => {
                    let reg_text = self.reg.as_ref().unwrap();
                    let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                    let rm_text = self.rm.as_ref().unwrap();
                    let (rm, is_memory) = match self.mode.as_ref() {
                        Some(mode) => match mode {
                            Mode::Mode11 => (
                                REGISTERS_W1.iter().position(|&r| r == rm_text).unwrap(),
                                false,
                            ),
                            _ => (self.get_memory_address(registers), true),
                        },
                        None => (
                            REGISTERS_W1.iter().position(|&r| r == rm_text).unwrap(),
                            false,
                        ),
                    };
                    if self.d.unwrap() {
                        if is_memory {
                            let memory_address = self.get_memory_address(registers);
                            let data = u16::from_le_bytes([
                                memory[memory_address],
                                memory[memory_address + 1],
                            ]) as i32;
                            let ea = self.get_ea();
                            *clocks += 8 + ea;

                            println!(
                                    "mov {}, {} ; Clocks: +{} {} (8 + {}) | {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x}",
                                    reg_text,
                                    rm_text,
                                    8 + ea,
                                    clocks,
                                    ea,
                                    reg_text,
                                    registers[reg],
                                    data,
                                    *prev_ip,
                                    *ip
                                );
                            registers[reg] = data;
                        } else {
                            println!(
                                "mov {}, {} ; {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x}",
                                reg_text,
                                rm_text,
                                reg_text,
                                registers[reg],
                                registers[rm],
                                *prev_ip,
                                *ip
                            );
                            registers[reg] = registers[rm]
                        }
                    } else if is_memory {
                        let ea = self.get_ea();
                        *clocks += 9 + ea;
                        println!(
                            "mov {} {} ; Clocks: +{} {} (9 + {}) |ip:0x{:x}->0x{:x}",
                            rm_text,
                            reg_text,
                            9 + ea,
                            clocks,
                            ea,
                            *prev_ip,
                            *ip,
                        );
                        let num = registers[reg];
                        let high: u8 = (num >> 8) as u8;
                        let low: u8 = (num & 0xFF) as u8;
                        let memory_address = self.get_memory_address(registers);
                        memory[memory_address] = low;
                        memory[memory_address + 1] = high;
                    } else {
                        *clocks += 2;
                        println!(
                            "mov {}, {} ; Clocks: +2 = {} | {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x}",
                            rm_text,
                            reg_text,
                            clocks,
                            rm_text,
                            registers[rm],
                            registers[reg],
                            *prev_ip,
                            *ip
                        );
                        registers[rm] = registers[reg]
                    }
                }
                OperandType::ImmRm => {
                    println!(
                        "mov {}, {}; ip:0x{:x}->0x{:x}",
                        self.rm.as_ref().unwrap(),
                        self.data.unwrap(),
                        *prev_ip,
                        *ip
                    );
                    let num = self.data.unwrap();
                    let high: u8 = (num >> 8) as u8;
                    let low: u8 = (num & 0xFF) as u8;
                    let memory_address = self.get_memory_address(registers);
                    memory[memory_address] = low;
                    memory[memory_address + 1] = high;
                }
                _ => {
                    println!("Unimplemented");
                }
            },
            "add" => {
                match self.oprand_type {
                    OperandType::ImmReg => {
                        let reg_text = self.reg.as_ref().unwrap();
                        let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                        let data = self.data.unwrap() as i32;
                        let result = registers[reg] + data;
                        let flags = update_flag(flag_registers, result);
                        *clocks += 4;
                        println!(
                            "add {}, {} ; Clocks: +4 = {} | {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x} {}",
                            reg_text,
                            data,
                            clocks,
                            reg_text,
                            registers[reg],
                            result,
                            *prev_ip,
                            *ip,
                            flags,
                        );
                        registers[reg] = result
                    }
                    OperandType::RegMem => {
                        // currently only supports register to register
                        let reg_text = self.reg.as_ref().unwrap();
                        let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                        let rm_text = self.rm.as_ref().unwrap();
                        let rm = REGISTERS_W1.iter().position(|&r| r == rm_text).unwrap();
                        if self.d.unwrap() {
                            let result = registers[reg] + registers[rm];
                            let flags = update_flag(flag_registers, result);
                            *clocks += 3;
                            println!(
                                "add {}, {} ; Clocks: +3 = {} | {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x} {}",
                                reg_text,
                                rm_text,
                                clocks,
                                reg_text,
                                registers[reg],
                                result,
                                *prev_ip,
                                *ip,
                                flags,
                            );
                            registers[reg] = result
                        } else {
                            let result = registers[rm] + registers[reg];
                            let flags = update_flag(flag_registers, result);
                            *clocks += 3;
                            println!(
                                "add {}, {} ; Clocks: +3 = {} | {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x} {}",
                                rm_text,
                                reg_text,
                                clocks,
                                rm_text,
                                registers[rm],
                                result,
                                *prev_ip,
                                *ip,
                                flags,
                            );
                            registers[rm] = result
                        }
                    }
                    _ => {
                        println!("Unimplemented");
                    }
                }
            }
            "sub" => {
                match self.oprand_type {
                    OperandType::ImmReg => {
                        let reg_text = self.reg.as_ref().unwrap();
                        let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                        let data = self.data.unwrap() as i32;
                        let result = registers[reg] - data;
                        let flags = update_flag(flag_registers, result);
                        println!(
                            "sub {}, {} ; {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x} {}",
                            reg_text, data, reg_text, registers[reg], result, *prev_ip, *ip, flags,
                        );
                        registers[reg] = result
                    }
                    OperandType::RegMem => {
                        // currently only supports register to register
                        let reg_text = self.reg.as_ref().unwrap();
                        let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                        let rm_text = self.rm.as_ref().unwrap();
                        let rm = REGISTERS_W1.iter().position(|&r| r == rm_text).unwrap();
                        if self.d.unwrap() {
                            let result = registers[reg] - registers[rm];
                            let flags = update_flag(flag_registers, result);
                            println!(
                                "sub {}, {} ; {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x} {}",
                                reg_text,
                                rm_text,
                                reg_text,
                                registers[reg],
                                result,
                                *prev_ip,
                                *ip,
                                flags,
                            );
                            registers[reg] = result
                        } else {
                            let result = registers[rm] - registers[reg];
                            let flags = update_flag(flag_registers, result);
                            println!(
                                "sub {}, {} ; {}:0x{:x}->0x{:x} ip:0x{:x}->0x{:x} {}",
                                rm_text,
                                reg_text,
                                rm_text,
                                registers[rm],
                                result,
                                *prev_ip,
                                *ip,
                                flags,
                            );
                            registers[rm] = result
                        }
                    }
                    _ => {
                        println!("Unimplemented");
                    }
                }
            }
            "cmp" => {
                match self.oprand_type {
                    OperandType::ImmReg => {
                        let reg_text = self.reg.as_ref().unwrap();
                        let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                        let data = self.data.unwrap() as i32;
                        let result = registers[reg] - data;
                        let flags = update_flag(flag_registers, result);
                        println!(
                            "cmp {}, {} ip:0x{:x}->0x{:x} {}",
                            reg_text, data, *prev_ip, *ip, flags,
                        );
                    }
                    OperandType::RegMem => {
                        // currently only supports register to register
                        let reg_text = self.reg.as_ref().unwrap();
                        let reg = REGISTERS_W1.iter().position(|&r| r == reg_text).unwrap();
                        let rm_text = self.rm.as_ref().unwrap();
                        let rm = REGISTERS_W1.iter().position(|&r| r == rm_text).unwrap();
                        if self.d.unwrap() {
                            let result = registers[reg] - registers[rm];
                            let flags = update_flag(flag_registers, result);
                            println!(
                                "cmp {}, {} ip:0x{:x}->0x{:x} {}",
                                reg_text, rm_text, *prev_ip, *ip, flags,
                            );
                        } else {
                            let result = registers[rm] - registers[reg];
                            let flags = update_flag(flag_registers, result);
                            println!(
                                "cmp {}, {} ip:0x{:x}->0x{:x} {}",
                                rm_text, reg_text, *prev_ip, *ip, flags,
                            );
                        }
                    }
                    _ => {
                        println!("Unimplemented");
                    }
                }
            }
            "jne" => {
                let data = match self.negative_data {
                    Some(data) => data,
                    None => self.data.unwrap() as i32,
                };
                let difference = *ip as i32 - *prev_ip as i32;
                let current_ip = *prev_ip;
                if flag_registers[0] == 0 {
                    *ip = (*ip as i32 + data) as usize;
                    *prev_ip = *ip;
                }
                println!(
                    "jne ${} ; ip:0x{:x}->0x{:x}",
                    data + difference,
                    current_ip,
                    *ip,
                );
            }
            _ => {
                println!("Unimplemented");
            }
        }
    }

    fn get_memory_address(&self, registers: &[i32]) -> usize {
        let re = Regex::new(r"([a-zA-Z]+)|(\d+)").unwrap();
        let mut address = 0;

        for cap in re.captures_iter(self.rm.as_ref().unwrap()) {
            if let Some(matched_str) = cap.get(1) {
                address += registers[REGISTERS_W1
                    .iter()
                    .position(|&r| r == matched_str.as_str())
                    .unwrap()] as usize;
            }
            if let Some(matched_int) = cap.get(2) {
                address += matched_int.as_str().parse::<i32>().unwrap() as usize;
            }
        }
        address
    }

    fn get_ea(&self) -> usize {
        let re = Regex::new(r"([a-zA-Z]+)|(\d+)").unwrap();
        let mut displacement = 0;
        let mut base = vec![];

        for cap in re.captures_iter(self.rm.as_ref().unwrap()) {
            if let Some(matched_str) = cap.get(1) {
                base.push(matched_str.as_str());
            }
            if let Some(_matched_int) = cap.get(2) {
                displacement += 1
            }
        }
        if displacement == 0 {
            if base.len() == 1 {
                5
            } else if (base[0] == "bp" && base[1] == "di") || (base[0] == "bx" && base[1] == "si") {
                7
            } else {
                8
            }
        } else if base.is_empty() {
            // we can explicity check for 0 rather than check for len
            6
        } else if base.len() == 1 {
            9
        } else if (base[0] == "bp" && base[1] == "di") || (base[0] == "bx" && base[1] == "si") {
            11
        } else {
            12
        }
    }
}

fn update_flag(flag_registers: &mut [u8], result: i32) -> String {
    let mut flags = String::new();
    if result == 0 {
        if flag_registers[0] == 0 {
            flag_registers[0] = 1;
            flags.push_str("flags:-> Z");
        }
    } else if flag_registers[0] == 1 {
        flag_registers[0] = 0;
        flags.push_str("flags:Z->");
    }

    if result as u16 & 0b1000_0000_0000_0000 != 0 {
        if flag_registers[1] == 0 {
            flag_registers[1] = 1;
            flags.push_str("flags:-> S");
        }
    } else if flag_registers[1] == 1 {
        flag_registers[1] = 0;
        flags.push_str("flags:S->");
    }
    flags
}
