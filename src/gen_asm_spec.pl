:- module(gen_asm_spec, [report/0]).

:- use_module(isa).
:- use_module(derive).

report :-
    format(';~n'),
    format('; SPRIND ISA Assembly Language Specification~n'),
    format(';~n~n'),
    format('#once~n~n'),
    isa:version(VMajor, VMinor, VPatch),
    format('#const ISA_VERSION = "~d.~d.~d"~n~n', [VMajor, VMinor, VPatch]),

    findall(Reg, isa:regname_uses(Reg, _), Regs),
    format('#ruledef Reg {~n'),
    maplist({Regs}/[Reg]>>(
      nth0(Idx, Regs, Reg),
      format('  ~|~w~t~3+=> 0b~|~`0t~2r~3+~n', [Reg, Idx])
    ), Regs),
    format('}~n~n'),


    derive:subr_byte_alignment(SubrAlign),
    format('#const SPRIND_SUBR_ALIGN = ~d~n~n', SubrAlign),
end.

ruledef(RuleName, Rules) :-
    format('#ruledef ~w {~n', [RuleName]),
    maplist([Lhs-Rhs]>>format('  ~w => ~w~n', [Lhs, Rhs]), Rules),
    format('}~n~n'),
end.

/*


#ruledef Instruction {
      call {addr: u16} => {
            assert(addr != 0xFFFF, "address 0xFFFF may not store the start of a subroutine")
            0b11 @ (addr << 3)`14
      }

      ; RRI
      lw {dst: Reg}, [{base: AdrReg}][{disp: s7}] => 0b10 @ 0`2 @ disp @ base @ dst
      lb {dst: Reg}, [{base: AdrReg}][{disp: s7}] => 0b10 @ 1`2 @ disp @ base @ dst
      sw [{base: AdrReg}][{disp: s7}], {src: Reg} => 0b10 @ 2`2 @ disp @ base @ src
      sb [{base: AdrReg}][{disp: s7}], {src: Reg} => 0b10 @ 3`2 @ disp @ base @ src

      ; RI
      lzi   {dst: Reg}, {imm: u8} => 0b001 @ 0`2 @ imm @ dst
      lsi   {dst: Reg}, {imm: s8} => 0b001 @ 1`2 @ imm @ dst
      lui   {dst: Reg}, {imm: i8} => 0b001 @ 2`2 @ imm @ dst
      auipc {dst: Reg}, {imm: i8} => 0b001 @ 3`2 @ imm @ dst

      ; I
      b {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 0`3 @ rel_lbl`11
      }
      bl {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 1`3 @ rel_lbl`11
      }
      be {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 2`3 @ rel_lbl`11
      }
      bg {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 3`3 @ rel_lbl`11
      }
      ; __ {abs_lbl: u12} => {
      ;       rel_lbl = (abs_lbl - $) >> 1
      ;       0b01 @ 4`3 @ rel_lbl`11
      ; }
      bge {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 5`3 @ rel_lbl`11
      }
      bne {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 6`3 @ rel_lbl`11
      }
      ble {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 7`3 @ rel_lbl`11
      }

      ; RRR
      lw {dst: Reg}, [{base: Reg}][{index: Reg}] => 0b0001 @ 0`3 @ dst @ base @ index
      lb {dst: Reg}, [{base: Reg}][{index: Reg}] => 0b0001 @ 1`3 @ dst @ base @ index
      sw [{base: Reg}][{index: Reg}], {src: Reg} => 0b0001 @ 2`3 @ src @ base @ index
      sb [{base: Reg}][{index: Reg}], {src: Reg} => 0b0001 @ 3`3 @ src @ base @ index
      add {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 4`3 @ dst @ src1 @ src2
      sub {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 5`3 @ dst @ src1 @ src2
      and {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 6`3 @ dst @ src1 @ src2
      or  {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 7`3 @ dst @ src1 @ src2

      ; Ri
      cmpi {src: Reg}, {imm: i4} => 0b00001 @  0`4 @ imm @ src
      ; __ {src: Reg}, {imm: i4} => 0b00001 @  1`4 @ imm @ src
      ; __ {src: Reg}, {imm: i4} => 0b00001 @  2`4 @ imm @ src
      ; __ {src: Reg}, {imm: i4} => 0b00001 @  3`4 @ imm @ src
      addi {dst: Reg}, {imm: u4} => 0b00001 @  4`4 @ imm @ dst
      subi {dst: Reg}, {imm: u4} => 0b00001 @  5`4 @ imm @ dst
      tbit {src: Reg}, {imm: u4} => 0b00001 @  6`4 @ imm @ src
      sbit {dst: Reg}, {imm: u4} => 0b00001 @  7`4 @ imm @ dst
      xori {dst: Reg}, {imm: u4} => 0b00001 @  8`4 @ imm @ dst
      ; __ {dst: Reg}, {imm: u4} => 0b00001 @  9`4 @ imm @ dst
      ; __ {dst: Reg}, {imm: u4} => 0b00001 @ 10`4 @ imm @ dst
      ; __ {dst: Reg}, {imm: u4} => 0b00001 @ 11`4 @ imm @ dst
      lsli {dst: Reg}, {imm: u4} => 0b00001 @ 12`4 @ imm @ dst
      lsri {dst: Reg}, {imm: u4} => 0b00001 @ 13`4 @ imm @ dst
      asri {dst: Reg}, {imm: u4} => 0b00001 @ 14`4 @ imm @ dst
      roti {dst: Reg}, {imm: s4} => 0b00001 @ 15`4 @ imm @ dst

      ; RR1
      cmp  {src1: Reg}, {src2: Reg}    => 0b000001 @  0`4 @ src1 @ src2
      jral {jmp_tgt: Reg}, {link: Reg} => 0b000001 @  1`4 @ jmp_tgt @ link
      exch {reg1: Reg}, {reg2: Reg}    => 0b000001 @  2`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @  3`4 @ reg1 @ reg2
      add  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  4`4 @ reg1 @ reg2
      sub  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  5`4 @ reg1 @ reg2
      and  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  6`4 @ reg1 @ reg2
      or   {reg1: Reg}, {reg2: Reg}    => 0b000001 @  7`4 @ reg1 @ reg2
      xor  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  8`4 @ reg1 @ reg2
      xnor {reg1: Reg}, {reg2: Reg}    => 0b000001 @  9`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 10`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 11`4 @ reg1 @ reg2
      mulstep {reg1: Reg}, {reg2: Reg} => 0b000001 @ 12`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 13`4 @ reg1 @ reg2
      nand {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 14`4 @ reg1 @ reg2
      nor  {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 15`4 @ reg1 @ reg2

      ; RR2
      pushw [{addr: Reg}], {src: Reg} => 0b0000001 @ 0`3 @ addr @ src
      pushb [{addr: Reg}], {src: Reg} => 0b0000001 @ 1`3 @ addr @ src
      popw  {dst: Reg}, [{addr: Reg}] => 0b0000001 @ 2`3 @ addr @ dst
      popw  {dst: Reg}, [{addr: Reg}] => 0b0000001 @ 3`3 @ addr @ dst
      ; __
      ; __
      ; __
      ; __

      ; R
      seb {reg: Reg}   => 0b00000001 @ 0`5 @ reg
      rvend {reg: Reg} => 0b00000001 @ 1`5 @ reg
      rf {reg: Reg}    => 0b00000001 @ 2`5 @ reg
      wf {reg: Reg}    => 0b00000001 @ 3`5 @ reg
      ; ...

      ; O
      NONEXE0 => 0b00000000 @ 0`8
      BREAK   => 0b00000000 @ 1`8
      HALT    => 0b00000000 @ 2`8
      UNIMPL  => 0b00000000 @ 3`8
      kcall   => 0b00000000 @ 4`8
      kret    => 0b00000000 @ 5`8
      inrd    => 0b00000000 @ 6`8
      inre    => 0b00000000 @ 7`8

      ; SYNTHETIC INSTRUCTIONS
      mov {dst: Reg}, {src: Reg} => asm{ or {dst}, {src} }
      clear {reg: Reg} => asm{ xor {reg}, {reg} }
      nop => asm{ or zero, zero }
      ret => asm{ jral ra, zero }
      jr {reg: Reg} => asm{ jral {reg}, zero }
      incr {reg: Reg} => asm{ addi {reg}, 1 }
      decr {reg: Reg} => asm{ subi {reg}, 1 }
      neg {reg: Reg} => asm{ sub {reg}, zero, {reg} }
      inv {reg: Reg} => asm{ xnor {reg}, zero }
      not {reg: Reg} => asm{ xori {reg}, 1 }
      NONEXE1 => 0xFFFF
}


#bankdef rom {
      #bits 8
      #outp 0
}

#const SPRIND_SUBR_ALIGN = 4
*/

end.
