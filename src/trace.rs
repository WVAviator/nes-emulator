use std::collections::HashMap;

use crate::cpu::AddressingMode;
use crate::cpu::{opcodes, Mem, CPU};

pub fn trace(cpu: &mut CPU) {
    //CFE3  A1 80     LDA ($80,X) @ 82 = 0300 = 5B    A:5A X:02 Y:69 P:25 SP:FB

    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

    let code = cpu.mem_read(cpu.program_counter);

    let opcode = opcodes
        .get(&code)
        .expect(&format!("OpCode {:x} is not recognized", code));

    let bytes: Vec<u8> = (0..opcode.len)
        .map(|i| cpu.mem_read(cpu.program_counter + (i as u16)))
        .collect();

    let dbg_pc = format!("{:0>4X}", cpu.program_counter);
    let mut code_string = String::new();
    for i in 0..opcode.len {
        code_string.push_str(&format!(
            "{:0>2X}",
            cpu.mem_read(cpu.program_counter + (i as u16))
        ));
        if i < opcode.len - 1 {
            code_string.push_str(" ");
        }
    }

    let instruction = match opcode.mode {
        AddressingMode::Immediate => format!("#${:0>2X} ", bytes[1]),
        AddressingMode::Relative => format!(
            "${:0>4X} ",
            cpu.program_counter
                .wrapping_add(2)
                .wrapping_add((bytes[1] as i8) as u16)
        ),
        AddressingMode::ZeroPage => {
            format!(
                "${:0>2X} = {:0>2X}",
                bytes[1],
                cpu.mem_read(bytes[1] as u16)
            )
        }
        AddressingMode::ZeroPage_X => {
            let addr = bytes[1].wrapping_add(cpu.register_x);
            format!(
                "${:0>2X},X @ {:0>2X} = {:0>2X}",
                bytes[1],
                addr,
                cpu.mem_read(addr as u16)
            )
        }
        AddressingMode::ZeroPage_Y => {
            let addr = bytes[1].wrapping_add(cpu.register_y);
            format!(
                "${:0>2X},Y @ {:0>2X} = {:0>2X}",
                bytes[1],
                addr,
                cpu.mem_read(addr as u16)
            )
        }
        AddressingMode::Absolute => match opcode.mnemonic {
            "JSR" | "JMP" => format!("${:0>2X}{:0>2X}", bytes[2], bytes[1]),
            _ => format!(
                "${:0>2X}{:0>2X} = {:0>2X}",
                bytes[2],
                bytes[1],
                cpu.mem_read(((bytes[2] as u16) << 8) | bytes[1] as u16)
            ),
            // "STY" => format!(
            //     "${:0>2X}{:0>2X} = {:0>2X}",
            //     bytes[2], bytes[1], cpu.register_y
            // ),
            // "STA" => format!(
            //     "${:0>2X}{:0>2X} = {:0>2X}",
            //     bytes[2], bytes[1], cpu.register_a
            // ),
            //_ => format!("${:0>2X}{:0>2X}", bytes[2], bytes[1]),
        },
        AddressingMode::Absolute_X => {
            let orig_addr = ((bytes[2] as u16) << 8) | bytes[1] as u16;
            let addr = orig_addr.wrapping_add(cpu.register_x as u16);
            format!(
                "${:0>4X},X @ {:0>4X} = {:0>2X}",
                orig_addr,
                addr,
                cpu.mem_read(addr)
            )
        }
        AddressingMode::Absolute_Y => {
            let orig_addr = ((bytes[2] as u16) << 8) | bytes[1] as u16;
            let addr = orig_addr.wrapping_add(cpu.register_y as u16);
            let pos = cpu.mem_read_u16(addr);
            format!(
                "${:0>4X},Y @ {:0>4X} = {:0>2X}",
                orig_addr,
                addr,
                cpu.mem_read(addr)
            )
        }
        AddressingMode::Indirect_X => {
            let ptr: u8 = (bytes[1] as u8).wrapping_add(cpu.register_x);
            let lo = cpu.mem_read(ptr as u16);
            let hi = cpu.mem_read(ptr.wrapping_add(1) as u16);
            let indirect_addr = (hi as u16) << 8 | (lo as u16);

            format!(
                "(${:0>2X},X) @ {:0>2X} = {:0>4X} = {:0>2X}",
                bytes[1],
                ptr,
                indirect_addr,
                cpu.mem_read(indirect_addr)
            )
        }
        AddressingMode::Indirect_Y => {
            let lo = cpu.mem_read(bytes[1] as u16);
            let hi = cpu.mem_read((bytes[1] as u8).wrapping_add(1) as u16);
            let deref_base = (hi as u16) << 8 | (lo as u16);
            let deref = deref_base.wrapping_add(cpu.register_y as u16);
            let value = cpu.mem_read(deref);

            format!(
                "(${:0>2X}),Y = {:0>4X} @ {:0>4X} = {:0>2X}",
                bytes[1], deref_base, deref, value
            )
        }
        AddressingMode::Accumulator => {
            format!("A")
        }
        AddressingMode::NoneAddressing => {
            if opcode.code == 0x6c {
                let addr = ((bytes[2] as u16) << 8 | bytes[1] as u16);
                let indirect_ref = if addr & 0x00FF == 0x00FF {
                    let lo = cpu.mem_read(addr);
                    let hi = cpu.mem_read(addr & 0xFF00);
                    (hi as u16) << 8 | (lo as u16)
                } else {
                    cpu.mem_read_u16(addr)
                };

                format!(
                    "(${:0>2X}{:0>2X}) = {:0>4X}",
                    bytes[2], bytes[1], indirect_ref
                )
            } else {
                format!("")
            }
        }
        _ => format!(""),
    };

    let current_values = format!(
        "A:{:0>2X} X:{:0>2X} Y:{:0>2X} P:{:0>2X} SP:{:0>2X}",
        cpu.register_a,
        cpu.register_x,
        cpu.register_y,
        cpu.status.value(),
        cpu.stack_pointer
    );

    println!(
        "{:<6}{:<10}{:<4}{:<28}{:<26}",
        dbg_pc, code_string, opcode.mnemonic, instruction, current_values
    );
}
