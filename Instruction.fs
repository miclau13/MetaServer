module Instruction

type IInstruction<'a> =
  abstract member Map : ('a->'b) -> IInstruction<'b>