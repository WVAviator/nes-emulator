use super::*;

#[test]
fn test_0xa9_lda_immediate_load_data() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x05, 0x00]); // Argument to LDA is 0x05
    assert_eq!(cpu.register_a, 5);
    assert!(cpu.status & 0b0000_0010 == 0); // The Zero Flag should not be set
    assert!(cpu.status & 0b1000_000 == 0); // The Negative Flag should not be set
}

#[test]
fn test_0xa9_lda_zero_flag() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x00, 0x00]); // Argument to LDA is 0x00
    assert!(cpu.status & 0b0000_0010 == 0b10); // The Zero Flag should be set
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
fn test_5_ops_working_together() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 0xc1);
}

#[test]
fn test_inx_overflow() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 1);
}
