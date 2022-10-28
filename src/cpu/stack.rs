pub struct Stack<'a> {
    pub pointer: u8,
    memory: &'a mut [u8; 0xFF],
}

impl<'a> Stack<'a> {
    pub fn new(memory: &'a mut [u8; 0xFF]) -> Self {
        Stack {
            pointer: 0xFE,
            memory,
        }
    }
}

impl<'a> Stack<'a> {
    pub fn push(&mut self, value: u8) {
        self.pointer -= 1;
        self.memory[self.pointer as usize] = value;

        if self.pointer < 0 {
            panic!("Stack overflow!");
        }
    }

    pub fn pop(&mut self) -> u8 {
        let value = self.memory[self.pointer as usize];
        self.memory[self.pointer as usize] = 0x00;
        self.pointer += 1;
        value
    }

    pub fn peek(&self) -> u8 {
        self.memory[self.pointer as usize]
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn initializes_new_stack() {
        let memory: [u8; 2048] = [0; 2048];
        let stack_memory: &mut [u8; 0xFF] = &mut [0; 0xFF];
        stack_memory.copy_from_slice(&memory[0x00..0xFF]);
        let stack = Stack::new(stack_memory);

        assert_eq!(stack.pointer, 0xFE);
    }

    #[test]
    fn pushing_value_to_stack_updates_memory() {
        let memory: [u8; 2048] = [0; 2048];
        let stack_memory: &mut [u8; 0xFF] = &mut [0; 0xFF];
        stack_memory.copy_from_slice(&memory[0x00..0xFF]);
        let mut stack = Stack::new(stack_memory);

        stack.push(5);
        assert_eq!(memory[stack.pointer as usize], 5);
        stack.push(116);
        assert_eq!(memory[stack.pointer as usize], 116);

        assert_eq!(stack.pointer, 0xFC);
    }
}
