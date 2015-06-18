Require Import myplug.

Definition Im_a_prod_too x := x = 4 -> True.

Lemma test x : True -> False -> Im_a_prod_too x.
Proof.
myintro T F E.
exact T.
Qed.