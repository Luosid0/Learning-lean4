----- Propositions, Proofs, and Indexing ------
--curry-howard correspondence: propositions as types, proofs as programs
theorem prop1 :2+3=5 := by
    rfl

theorem prop2 : 15-8=7 := by
    simp

theorem prop3 : String.append "Hello, " "world!" = "Hello, world!" := by
    decide

def fifth (xs:List α)(ok:xs.length>=5) : α := xs[4]
