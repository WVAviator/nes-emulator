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
fn test_0xa1_lda_indirect_x() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x01, 0x05);
    cpu.mem_write(0x02, 0x07);
    cpu.mem_write(0x0705, 0x0a);
    cpu.load_and_run(vec![0xa2, 0x01, 0xa1, 0x00, 0x00]);

    assert_eq!(cpu.register_a, 0x0a);
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
    cpu.load_and_run(vec![0xa2, 0xee, 0x8e, 0x45, 0x23, 0x00]);

    assert_eq!(cpu.mem_read(0x2345), 0xee);
}

#[test]
fn test_0x94_sty_zero_page_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa2, 0x01, 0xa0, 0xed, 0x94, 0x01, 0x00]);

    assert_eq!(cpu.mem_read(0x0002), 0xed);
}

#[test]
fn test_0xaa_tax_move_a_to_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

    assert_eq!(cpu.register_x, 10);
}

#[test]
fn test_0xe8_inx_increment_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x00, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 1);
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
