package sml

class OutInstruction(label: String, op: String, register: Int) extends Instruction(label, op)
{
  override def execute(m: Machine): Unit =
  {
    println(m.regs(register))
  }

  override def toString(): String = 
  {
    super.toString + " register " + register
  }
}

object OutInstruction 
{
  def apply(label: String, register: Int) =
    new OutInstruction(label, "out", register)
}
