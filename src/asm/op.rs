// 16-bit registers
pub enum Reg16 {
    /// Accumulator and flags
    AF,
    BC,
    DE,
    HL,
    /// Stack pointer
    SP,
    /// Program counter
    PC,
}

// 8-bit registers
pub enum Reg8 {
    /// Accumulator
    A,
    B, C,
    D, E,
    H, L,
}

#[allow(non_camel_case_types)]
pub enum Op {
    // 8-bit load operation
    /// Load 8-bit value to register
    ld_r_n(Reg8, u8),
    /// Load 8-bit register to register
    ld_r_r(Reg8, Reg8),
    /// Load address at HL to 8-bit register
    ld_r_iHL(Reg8),
    /// Load 8-bit register to address at HL
    ld_iHL_r(Reg8),
    /// Load 8-bit value to address at HL
    ld_iHL_n(u8),
    /// Load 8-bit register to A
    ld_A_r(Reg8),
    /// Load address at BC to A
    ld_A_iBC,
    /// Load address at DE to A
    ld_A_iDE,
    /// Load address at HL to A
    ld_A_iHL,
    /// Load address to A
    ld_A_inn(u16),
    /// Load 8-bit value to A
    ld_A_n(u8),
    /// Load A to 8-bit register
    ld_r_A(Reg8),
    /// Load A to address at BC
    ld_iBC_A,
    /// Load A to address at DE
    ld_iDE_A,
    /// Load A to address at HL
    ld_iHL_A,
    /// Load A to address
    ld_inn_A(u16),
    /// Load IO port C (FF00+C) to A
    ld_A_ioC,
    /// Load A to IO port C (FF00+C)
    ld_ioC_A,
    /// Load IO port (FF00+n) to A
    ld_A_ion(u8),
    /// Load A to IO port (FF00+n)
    ld_ion_A(u8),
    /// Load address at HL to A and decrement HL
    ldd_A_iHL,
    /// Load A to address at HL and decrement HL
    ldd_iHL_A,
    /// Load address at HL to A and increment HL
    ldi_A_iHL,
    /// Load A to address at HL and increment HL
    ldi_iHL_A,

    // 16-bit load operations
    /// Load 16-bit value to register
    ld_rr_nn(Reg16, u16),
    /// Load HL to SP
    ld_SP_HL,
    /// Load SP+value to HL
    ldhl_SP_n(i8),
    /// Load SP to address
    ld_inn_SP(u16),
    /// Push 16-bit register to stack
    push(Reg16),
    /// Pop stack to 16-bit register
    pop(Reg16),

    // 8-bit arithmetic/logic operations
    /// Add 8-bit register to A
    add_A_r(Reg8),
    /// Add address at HL to A
    add_A_iHL,
    /// Add 8-bit value to A
    add_A_n(u8),
    /// Add 8-bit register plus carry flag to A
    adc_A_r(Reg8),
    /// Add address at HL plus carry flag to A
    adc_A_iHL,
    /// Add value plus carry flag to A
    adc_A_n(u8),

    /// Substract 8-bit register from A
    sub_r(Reg8),
    /// Substract address at HL from A
    sub_iHL,
    /// Substract 8-bit value from A
    sub_n(u8),
    /// Substract 8-bit register plus carry flag from A
    sbc_A_r(Reg8),
    /// Substract address at HL plus carry from A
    sbc_A_iHL,
    /// Substract 8-bit value plus carry from A
    sbc_A_n(u8),

    /// AND 8-bit register with A and store in A
    and_r(Reg8),
    /// AND address at HL with A and store in A
    and_iHL,
    /// AND 8-bit value with A and store in A
    and_n(u8),

    /// OR 8-bit register with A and store in A
    or_r(Reg8),
    /// OR address at HL with A and store in A
    or_iHL,
    /// OR 8-bit value with A and store in A
    or_n(u8),

    /// XOR 8-bit register with A and store in A
    xor_r(Reg8),
    /// XOR address at HL with A and store in A
    xor_iHL,
    /// XOR 8-bit value with A and store in A
    xor_n(u8),

    /// Compare 8-bit register with A
    cp_r(Reg8),
    /// Compare address at HL with A
    cp_iHL,
    /// Compare 8-bit value with A
    cp_n(u8),

    /// Increment 8-bit register
    inc_r(Reg8),
    /// Increment address at HL
    inc_iHL,
    /// Decrement 8-bit register
    dec_r(Reg8),
    /// Decrement address at HL
    dec_iHL,

    // 16-bit arithmetic operations
    /// Add 16-bit register to HL
    add_HL_rr(Reg16),
    /// Add 8-bit value to SP
    add_SP_n(u8),
    /// Increment 16-bit register
    inc_rr(Reg16),
    /// Decrement 16-bit register
    dec_rr(Reg16),

    // Miscellaneous operations
    /// Swap nibbles of 8-bit register
    swap_r(Reg8),
    /// Swap nibbles of address at HL
    swap_iHL,
    /// Decimal adjust A
    daa,
    /// Complement A
    cpl,
    /// Complement carry flag
    ccf,
    /// Set carry flag
    scf,
    /// No operation
    nop,
    /// Power down CPU until interrupt
    halt,
    /// Low power standby mode until button press
    stop,
    /// Disable interrupts
    di,
    /// Enable interrupts
    ei,

    // Rotate and shift operations
    /// Rotate left A
    rlca,
    /// Rotate left A through carry flag
    rla,
    /// Rotate right A
    rrca,
    /// Rotate right A through carry flag
    rra,

    /// Rotate left 8-bit register
    rlc_r(Reg8),
    /// Rotate left address at HL
    rlc_iHL,
    /// Rotate left 8-bit register through carry flag
    rl(Reg8),
    /// Rotate left address at HL through carry flag
    rl_iHL,
    /// Rotate right 8-bit register
    rrc_r(Reg8),
    /// Rotate right address at HL
    rrc_iHL,
    /// Rotate right 8-bit register through carry flag
    rr_r(Reg8),
    /// Rotate right address at HL through carry flag
    rr_iHL,

    /// Shift left 8-bit register (arithmetic)
    sla_r(Reg8),
    /// Shift left address at HL (arithmetic)
    sla_iHL,
    /// Shift right 8-bit register (arithmetic)
    sra_r(Reg8),
    /// Shift right address at HL (arithmetic)
    sra_iHL,
    /// Shift right 8-bit register (logical)
    srl_r(Reg8),
    /// Shift right address at HL (logical)
    srl_iHL,

    // Bitwise operations
    /// Test bit in 8-bit register
    bit_r(u8, Reg8),
    /// Test bit in address at HL
    bit_iHL(u8),
    /// Set bit in 8-bit register
    set_r(u8, Reg8),
    /// Set bit in address at HL
    set_iHL(u8),
    /// Reset bit in 8-bit register
    res_r(u8, Reg8),
    /// Reset bit in address at HL
    res_iHL(u8),

    // Jump operations
    /// Jump to address
    jp(u16),
    /// Jump to address if Z flag is reset
    jp_NZ(u16),
    /// Jump to address if Z flag is set
    jp_Z(u16),
    /// Jump to address if C flag is reset
    jp_NC(u16),
    /// Jump to address if C flag is set
    jp_C(u16),
    /// Jump to address at HL
    jp_iHL,

    /// Jump to relative address
    jr(i8),
    /// Jump to relative address if Z flag is reset
    jr_NZ(i8),
    /// Jump to relative address if Z flag is set
    jr_Z(i8),
    /// Jump to relative address if C flag is reset
    jr_NC(i8),
    /// Jump to relative address if C flag is set
    jr_C(i8),

    // Call operations
    /// Push next address to stack and jump to address
    call(u16),
    /// Push next address to stack and jump to address if Z flag is reset
    call_NZ(u16),
    /// Push next address to stack and jump to address if Z flag is set
    call_Z(u16),
    /// Push next address to stack and jump to address if C flag is reset
    call_NC(u16),
    /// Push next address to stack and jump to address if C flag is set
    call_C(u16),

    // Restart operations
    /// Push current address to stack and jump to 0x00
    rst_00,
    /// Push current address to stack and jump to 0x08
    rst_08,
    /// Push current address to stack and jump to 0x10
    rst_10,
    /// Push current address to stack and jump to 0x18
    rst_18,
    /// Push current address to stack and jump to 0x20
    rst_20,
    /// Push current address to stack and jump to 0x28
    rst_28,
    /// Push current address to stack and jump to 0x30
    rst_30,
    /// Push current address to stack and jump to 0x38
    rst_38,

    // Return operations
    /// Pop address from stack and jump
    ret,
    /// Pop address from stack and jump if Z flag is reset
    ret_NZ,
    /// Pop address from stack and jump if Z flag is set
    ret_Z,
    /// Pop address from stack and jump if C flag is reset
    ret_NC,
    /// Pop address from stack and jump if C flag is set
    ret_C,
    /// Pop address from stack, jump and enable interrupts
    reti,
}
