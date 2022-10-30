use super::*;

#[test]
fn test_0xa9_lda_immediate() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
    assert_eq!(cpu.register_a, 5);
    assert!(!cpu.status.get(Flag::Z));
    assert!(!cpu.status.get(Flag::N));
}

#[test]
fn test_0xa5_lda_zero_page() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x03, 0x01); //set addr 3 to 0
    cpu.load_and_run(vec![0xa5, 0x03, 0x00]); // Set x to 1, set a to val at 1 + 2 (0)
    assert_eq!(cpu.register_a, 0x01);
}

#[test]
fn test_0xa2_ldx_immediate() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0xfd, 0x00]);

    assert_eq!(cpu.register_x, 0xfd);
    assert_eq!(cpu.status.get(Flag::N), true);
    assert!(!cpu.status.get(Flag::Z));
}

#[test]
fn test_0xa0_ldy_immediate() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa0, 0x09, 0x00]);

    assert_eq!(cpu.register_y, 0x09);
    assert!(!cpu.status.get(Flag::Z));
    assert!(!cpu.status.get(Flag::N));
}

#[test]
fn test_0x8d_sta_absolute() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x01, 0x8d, 0x34, 0x12, 0x00]);

    assert_eq!(cpu.mem_read(0x1234), 0x01);
}

#[test]
fn test_0x85_sta_zero_page() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x01, 0x85, 0x05, 0x00]);

    assert_eq!(cpu.mem_read(0x0005), 0x01);
}

#[test]
fn test_0x8e_stx_absolute() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0xee, 0x8e, 0x34, 0x12, 0x00]);

    assert_eq!(cpu.mem_read(0x1234), 0xee);
}

#[test]
fn test_0x94_sty_zero_page_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0x01, 0xa0, 0xed, 0x94, 0x01, 0x00]);

    assert_eq!(cpu.mem_read(0x0002), 0xed);
}

#[test]
fn test_0xaa_tax_transfer_a_to_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

    assert_eq!(cpu.register_x, 0x0a);
}

#[test]
fn test_0xba_tsx_transfer_stack_to_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xba, 0x00]);

    assert_eq!(cpu.register_x, STACK_RESET);
}

#[test]
fn test_0xa8_tay_transfer_a_to_y() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xff, 0xa8, 0x00]);

    assert_eq!(cpu.register_y, 0xff);
    assert!(cpu.status.get(Flag::N));
}

#[test]
fn test_0x8a_txa_transfer_x_to_a() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0x0a, 0x8a, 0x00]);

    assert_eq!(cpu.register_a, 0x0a);
}

#[test]
fn test_0x9a_txs_transfer_x_to_stack() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0x0a, 0x9a, 0x00]);

    assert_eq!(cpu.stack_pointer, 0x0a);
}

#[test]
fn test_0x98_txa_transfer_y_to_a() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa0, 0x0a, 0x98, 0x00]);

    assert_eq!(cpu.register_a, 0x0a);
}

#[test]
fn test_0xe8_inx() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0x05, 0xe8, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 0x07);
}

#[test]
fn test_inx_overflow() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 1);
}

#[test]
fn test_0x69_adc_immediate() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x0a, 0x69, 0x07, 0x00]); // load 10 to a and then add 7

    assert_eq!(cpu.register_a, 0x11); // expect 17
}

#[test]
fn test_0x65_adc_zero_page() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x0055, 0xfa);
    cpu.load_and_run(vec![0xa9, 0x0a, 0x65, 0x55, 0x00]); // load 10 to a then read zero page 0x55 and add to a

    assert_eq!(cpu.register_a, 0x04); //wraps around to 4 and sets carry flag
    assert_eq!(cpu.status.get(Flag::C), true);
}

#[test]
fn test_0x75_adc_zero_page_x() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x31, 0x05); // store 5 at 49
    cpu.load_and_run(vec![0xa9, 0x02, 0xa2, 0x01, 0x75, 0x30]); // a = 2, x = 1, add 1 + 48 for addr, add 5 to 2

    assert_eq!(cpu.register_a, 0x07);
}

#[test]
fn test_0x79_adc_absolute_y() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x1234, 0x05);
    // Load A with 1, Load Y with 2, fetch 5 from 0x1234 by adding 0x1232 (le) to y (2)
    cpu.load_and_run(vec![0xa9, 0x01, 0xa0, 0x02, 0x79, 0x32, 0x12, 0x00]);

    assert_eq!(cpu.register_a, 0x06);
}

#[test]
fn test_0x2d_and_absolute() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x1234, 0b1100_1011);
    cpu.load_and_run(vec![0xa9, 0b1100_1100, 0x2d, 0x34, 0x12, 0x00]);

    assert_eq!(cpu.register_a, 0b1100_1000);
}

#[test]
fn test_0x35_and_zero_page_x() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x34, 0b1111_1011);
    cpu.load_and_run(vec![0xa9, 0b1100_1101, 0xa2, 0x31, 0x35, 0x03, 0x00]);

    assert_eq!(cpu.register_a, 0b1100_1001);
}

#[test]
fn test_0x0a_asl_accumulator() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x14, 0x0a, 0x00]);

    assert_eq!(cpu.register_a, 0x28);
}

#[test]
fn test_0x06_asl_zero_page() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x45, 0x80);
    cpu.load_and_run(vec![0x06, 0x45, 0x00]);

    assert_eq!(cpu.mem_read(0x45), 0x00);
    assert!(cpu.status.get(Flag::Z));
    assert!(cpu.status.get(Flag::C));
}

#[test]
fn test_0x90_bcc() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0x90, 0x02, 0xa9, 0x01, 0x00]); // skip over setting a
    assert_eq!(cpu.register_a, 0x00);

    cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0xff, 0x90, 0x02, 0xa9, 0x00, 0x00]);
    assert_eq!(cpu.register_a, 0x00);
}

#[test]
fn test_0xb0_bcs() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xb0, 0x02, 0xa9, 0x01, 0x00]);
    assert_eq!(cpu.register_a, 0x01);

    cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x02, 0xb0, 0x02, 0xa9, 0x00, 0x00]);
    assert_eq!(cpu.register_a, 0x01);
}

#[test]
fn test_0xf0_beq() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x00, 0xf0, 0x02, 0xa9, 0x01, 0x00]);
    assert_eq!(cpu.register_a, 0x00);

    cpu.load_and_run(vec![0xa9, 0x01, 0xf0, 0x02, 0xa9, 0x90, 0x00]);
    assert_eq!(cpu.register_a, 0x90);
}

#[test]
fn test_0x30_bmi() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xff, 0x30, 0x02, 0xa9, 0x22, 0x00]);
    assert_eq!(cpu.register_a, 0xff);

    cpu.load_and_run(vec![0xa9, 0x01, 0x30, 0x02, 0xa9, 0x22, 0x00]);
    assert_eq!(cpu.register_a, 0x22);
}

#[test]
fn test_0xd0_bne() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x01, 0xd0, 0x02, 0xa9, 0x30, 0x00]);
    assert_eq!(cpu.register_a, 0x01);

    cpu.load_and_run(vec![0xa9, 0x00, 0xd0, 0x02, 0xa9, 0x30, 0x00]);
    assert_eq!(cpu.register_a, 0x30);
}

#[test]
fn test_0x10_bpl() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xff, 0x10, 0x02, 0xa9, 0x22, 0x00]);
    assert_eq!(cpu.register_a, 0x22);

    cpu.load_and_run(vec![0xa9, 0x01, 0x10, 0x02, 0xa9, 0x22, 0x00]);
    assert_eq!(cpu.register_a, 0x01);
}

#[test]
fn test_0x50_bvc() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x7f, 0x69, 0x02, 0x50, 0x02, 0xa9, 0xfe, 0x00]);
    assert_eq!(cpu.register_a, 0xfe);

    cpu.load_and_run(vec![0xa9, 0x7d, 0x69, 0x02, 0x50, 0x02, 0xa9, 0xfe, 0x00]);
    assert_eq!(cpu.register_a, 0x7f);
}

#[test]
fn test_0x70_bvs() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x7f, 0x69, 0x02, 0x70, 0x02, 0xa9, 0xfe, 0x00]);
    assert_eq!(cpu.register_a, 0x81);

    cpu.load_and_run(vec![0xa9, 0x7d, 0x69, 0x02, 0x70, 0x02, 0xa9, 0xfe, 0x00]);
    assert_eq!(cpu.register_a, 0xfe);
}

#[test]
fn test_0x24_bit_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x50, 0b1010_1010);
    cpu.load_and_run(vec![0xa9, 0b0000_1111, 0x24, 0x50, 0x00]);

    assert_eq!(cpu.status.get(Flag::Z), false);
    assert_eq!(cpu.status.get(Flag::V), false);
    assert_eq!(cpu.status.get(Flag::N), true);
}

#[test]
fn test_0x2c_bit_absolute() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x1234, 0b1100_1100);
    cpu.load_and_run(vec![0xa9, 0b0011_0011, 0x2c, 0x34, 0x12, 0x00]);

    assert_eq!(cpu.status.get(Flag::Z), true);
    assert_eq!(cpu.status.get(Flag::V), true);
    assert_eq!(cpu.status.get(Flag::N), true);
}

#[test]
fn test_0x18_clc() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x01, 0x18, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), false);
}

#[test]
fn test_0xb8_clv() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x7f, 0x69, 0x01, 0xb8, 0x00]);
    assert_eq!(cpu.status.get(Flag::V), false);
}

#[test]
fn test_0xc9_cmp_immediate() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x02, 0xc9, 0x01, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), true);
    assert_eq!(cpu.status.get(Flag::Z), false);
    assert_eq!(cpu.status.get(Flag::N), false);
}

#[test]
fn test_0xc5_cmp_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x05, 0x06);
    cpu.load_and_run(vec![0xa9, 0x06, 0xc5, 0x05, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), true);
    assert_eq!(cpu.status.get(Flag::Z), true);
    assert_eq!(cpu.status.get(Flag::N), false);
}

#[test]
fn test_0xcd_cmp_absolute() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x1234, 0x01);
    cpu.load_and_run(vec![0xa9, 0xff, 0xcd, 0x34, 0x12, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), true);
    assert_eq!(cpu.status.get(Flag::Z), false);
    assert_eq!(cpu.status.get(Flag::N), true);
}

#[test]
fn test_0xe0_cpx_immediate() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa2, 0x05, 0xe0, 0x05, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), true);
    assert_eq!(cpu.status.get(Flag::Z), true);
    assert_eq!(cpu.status.get(Flag::N), false);
}

#[test]
fn test_0xc4_cpy_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0xde, 0x04);
    cpu.load_and_run(vec![0xa0, 0x05, 0xc4, 0xde, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), true);
    assert_eq!(cpu.status.get(Flag::Z), false);
    assert_eq!(cpu.status.get(Flag::N), false);
}

#[test]
fn test_0xce_dec_absolute() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x1234, 0x10);
    cpu.load_and_run(vec![0xce, 0x34, 0x12, 0x00]);

    assert_eq!(cpu.mem_read(0x1234), 0x0f);
}

#[test]
fn test_0xde_dec_absolute_x() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x1234, 0x10);
    cpu.load_and_run(vec![0xa2, 0x02, 0xde, 0x32, 0x12, 0x00]);

    assert_eq!(cpu.mem_read(0x1234), 0x0f);
}

#[test]
fn test_0xca_dex() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa2, 0x0a, 0xca, 0x00]);

    assert_eq!(cpu.register_x, 0x09);
}

#[test]
fn test_0x88_dey() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa0, 0x0a, 0x88, 0x00]);

    assert_eq!(cpu.register_y, 0x09);
}

#[test]
fn test_0x45_eor_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x30, 0b1010_1010);
    cpu.load_and_run(vec![0xa9, 0b1100_1100, 0x45, 0x30, 0x00]);

    assert_eq!(cpu.register_a, 0b0110_0110);
}

#[test]
fn test_0xe6_inc_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0xf0, 0xdd);
    cpu.load_and_run(vec![0xe6, 0xf0, 0x00]);

    assert_eq!(cpu.mem_read(0xf0), 0xde);
}

#[test]
fn test_0xc8_iny() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa0, 0xf0, 0xc8, 0x00]);

    assert_eq!(cpu.register_y, 0xf1);
}

#[test]
fn test_0x4c_jmp_absolute() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![
        0xa9, 0x03, 0x4c, 0x08, 0x80, 0x00, 0x00, 0x00, 0x8d, 0x00, 0x02, 0x00,
    ]);

    assert_eq!(cpu.mem_read(0x0200), 0x03);
}

#[test]
fn test_0x6c_jmp_indirect() {
    let mut cpu = CPU::new();

    cpu.mem_write_u16(0x1234, 0x8008);
    cpu.load_and_run(vec![
        0xa9, 0x03, 0x6c, 0x34, 0x12, 0x00, 0x00, 0x00, 0x8d, 0x00, 0x02, 0x00,
    ]);

    assert_eq!(cpu.mem_read(0x0200), 0x03);
}

#[test]
fn test_0x6c_jmp_indirect_page_bug() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x12FF, 0x08);
    cpu.mem_write(0x1200, 0x80);

    cpu.load_and_run(vec![
        0xa9, 0x03, 0x6c, 0xff, 0x12, 0x00, 0x00, 0x00, 0x8d, 0x00, 0x02, 0x00,
    ]);

    assert_eq!(cpu.mem_read(0x0200), 0x03);
}

#[test]
fn test_0x20_jsr() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0x20, 0x05, 0x80, 0xa9, 0x05, 0xa2, 0x01, 0x00]);

    assert_eq!(cpu.register_a, 0x00);
    assert_eq!(cpu.register_x, 0x01);
    assert_eq!(cpu.stack_pop_u16(), 0x8002);
}

#[test]
fn test_0x60_rts() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![
        0xa2, 0x05, 0x20, 0x08, 0x80, 0xa9, 0x01, 0x00, 0xe8, 0xe8, 0x60, 0xa9, 0x02,
    ]);

    assert_eq!(cpu.register_a, 0x01);
    assert_eq!(cpu.register_x, 0x07);
    assert_eq!(cpu.stack_pointer, STACK_RESET);
}

#[test]
fn test_0x4a_lsr_accumulator() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0b0110_1001, 0x4a, 0x00]);

    assert_eq!(cpu.register_a, 0b0011_0100);
    assert_eq!(cpu.status.get(Flag::C), true);
}

#[test]
fn test_0x46_lsr_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x99, 0b1100_1100);
    cpu.load_and_run(vec![0x46, 0x99, 0x00]);

    assert_eq!(cpu.mem_read(0x99), 0b0110_0110);
    assert_eq!(cpu.status.get(Flag::C), false);
}

#[test]
fn test_0xea_nop() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xea, 0xea, 0xa9, 0x02, 0xea, 0x00]);

    assert_eq!(cpu.register_a, 0x02);
}

#[test]
fn test_0x09_ora_immediate() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0b1001_1001, 0x09, 0b0110_1010, 0x00]);

    assert_eq!(cpu.register_a, 0b1111_1011);
    assert_eq!(cpu.status.get(Flag::N), true);
}

#[test]
fn test_0x0d_ora_absolute() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x0123, 0b0001_1110);
    cpu.load_and_run(vec![0xa9, 0b0100_0000, 0x0d, 0x23, 0x01, 0x00]);

    assert_eq!(cpu.register_a, 0b0101_1110); // 0110_0011
    assert_eq!(cpu.status.get(Flag::N), false);
}

#[test]
fn test_0x48_pha() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xdd, 0x48, 0x00]);

    assert_eq!(cpu.mem_read(0x0100 | (cpu.stack_pointer + 1) as u16), 0xdd);
    assert_eq!(cpu.stack_pop(), 0xdd);
}

#[test]
fn test_0x08_php() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x01, 0x08, 0x00]);

    let status = cpu.stack_pop();
    assert!(status & 0b0000_0011 == 0b0000_0011);
    assert!(cpu.status.get(Flag::Z));
    assert!(cpu.status.get(Flag::C));
}

#[test]
fn test_0x68_pla() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xff, 0x48, 0xa9, 0xdd, 0x68, 0x00]);

    assert_eq!(cpu.register_a, 0xff);
}

#[test]
fn test_0x28_plp() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x00, 0x08, 0xa9, 0x01, 0x28, 0x00]);

    assert_eq!(cpu.status.get(Flag::Z), true);
}

#[test]
fn test_0x2a_rol_accumulator() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x04, 0x2a, 0x00]);

    assert_eq!(cpu.register_a, 0x07);
    assert_eq!(cpu.status.get(Flag::C), false);
}

#[test]
fn test_0x26_rol_zero_page() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x69, 0xff);
    cpu.load_and_run(vec![0x26, 0x69, 0x00]);

    assert_eq!(cpu.mem_read(0x69), 0xfe);
    assert_eq!(cpu.status.get(Flag::C), true);
}

#[test]
fn test_0x6a_ror_accumulator() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0b1111_0001, 0x6a, 0x00]);

    assert_eq!(cpu.register_a, 0b0111_1000);
    assert_eq!(cpu.status.get(Flag::C), true);
}

#[test]
fn test_0x6e_ror_absolute() {
    let mut cpu = CPU::new();

    cpu.mem_write(0x1234, 0b0001_1000);
    cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x01, 0x6e, 0x34, 0x12, 0x00]);

    assert_eq!(cpu.mem_read(0x1234), 0b1000_1100);
    assert_eq!(cpu.status.get(Flag::C), false);
}

#[test]
fn test_0x40_rti() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![
        0xa2, 0xf0, 0x20, 0x06, 0x80, 0x00, 0x08, 0xa2, 0x05, 0x40,
    ]);

    assert_eq!(cpu.register_x, 0x05);
    assert_eq!(cpu.status.get(Flag::N), true);
}

#[test]
fn test_0xe9_sbc_immediate() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x03, 0xe9, 0x04, 0x00]);

    assert_eq!(cpu.register_a, 0xfe);
    assert_eq!(cpu.status.get(Flag::N), true);
}

#[test]
fn test_0xe5_sbc_zero_page() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xa9, 0x50, 0x85, 0xa2, 0xa9, 0x59, 0xe5, 0xa2, 0x00]);

    assert_eq!(cpu.register_a, 0x08);
    assert_eq!(cpu.status.get(Flag::C), true);
}

#[test]
fn test_0x38_sec() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0x38, 0x00]);
    assert_eq!(cpu.status.get(Flag::C), true);
}

#[test]
fn test_0xf8_sed() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0xf8, 0x00]);
    assert_eq!(cpu.status.get(Flag::D), true);
}

#[test]
fn test_0x78_sei() {
    let mut cpu = CPU::new();

    cpu.load_and_run(vec![0x78, 0x00]);
    assert_eq!(cpu.status.get(Flag::I), true);
}
