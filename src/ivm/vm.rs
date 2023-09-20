use std;

#[derive(Default, Clone)]
struct Misc {
    /// Contains the remainder of modulo division operation
    remainder: usize,
    /// Contains the result of the last comparison operation
    equal_flag: bool,
}






























impl Misc {
    pub fn new() -> Misc { Misc { remainder:  0, equal_flag: false } }
}

/// VM represents the Ivy Virtual Machine that executes bytecode.
#[derive(Default, Clone)]
pub struct VM {
    /// The virtual machine has 32 registers.
    pub registers: [i32; 32],
    /// Program counter keeps track of which byte is being executed.
    pc: usize,
    /// Stack pointer keeps track of the current location in the stack.
    sp: usize,
    /// The bytecode of the program being run.
    pub bytecode: Vec<u8>,
    /// Holds for heap memory.
    heap: Vec<u8>,
    /// Represents the stack.
    stack: Vec<i32>,
    //// Misillanious info.
    misc: Misc,
    /// Contains the read-only section data.
    read_data: Vec<u8>,
}


impl VM {
    /// Creates and returns a new virtual machine.
    pub fn new() -> VM {
        VM {
            registers:  [0; 32],
            pc:         0,
            sp:         0,
            bytecode:   vec![],
            heap:       vec![],
            stack:      vec![],
            misc:       Misc::new(),
            read_data:  vec![],
        }
    }
}