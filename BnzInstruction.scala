package sml

class BnzInstruction(label: String, op: String, register: Int, label2: String) extends Instruction(label, op)
{
  override def execute(m: Machine): Unit =
  {
    if (m.regs(register) != 0)
      m.labels.apply(m.prog.indexOf(label2))
  }
        
  override def toString(): String = 
  {
    super.toString + " register " + register + " will execute the following statement next: " + label2
  }
}

object BnzInstruction 
{
  def apply(label: String, register: Int, label2: String) =
    new BnzInstruction(label, "bnz", register, label2)
}