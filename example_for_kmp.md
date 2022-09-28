# Example for KMP algorithm

### KMP algorithm

```C
#define ARRAY_SIZE 50	
	char p[ARRAY_SIZE];
    long long next[ARRAY_SIZE];
    long long n = ctx -> data;
    long long i = 0, j = -1;
    next[0] = -1;
    /*Inv :  forall, 0 <= i <= n, -1 <= next[i] < i */
    while (i < n)
    {
      if (j == -1 || p[i] == p[j])
      {
         ++i; ++j; next[i] = j;
      }
      else j = next[j];
    }
```

###  Example for kmp

Here we show the kmp program with assertion. In canonical form, the loop invariant will be $\exists x \ y. PROP(\forall 0 \leq k \leq x, -1 \leq l_1[k] \leq k )LOCAL(temp \ i\  x;temp \ j\  y)SEP(Memory \ next \ l_1; Memory \ p \ l_2)$ When we first execute the loop body, the $x = 0 \wedge y = -1$. When we end the loop $x = n \wedge y = l_1[i]$. And the $l_1$ will be changed during the entire execution of the loop.

```bash
0000000000000000 <bpf_prog1>:
       0:       61 11 00 00 00 00 00 00 r1 = *(u32 *)(r1 + 0)
       1:       b7 02 00 00 ff ff ff ff r2 = -1
       2:       7b 2a 38 fe 00 00 00 00 *(u64 *)(r10 - 456) = r2
       3:       15 01 1a 00 00 00 00 00 if r1 == 0 goto +26 <LBB0_6>
       4:       b7 03 00 00 00 00 00 00 r3 = 0

0000000000000028 <LBB0_2>:
       5:       15 02 08 00 ff ff ff ff if r2 == -1 goto +8 <LBB0_4>
       6:       bf a4 00 00 00 00 00 00 r4 = r10
       7:       07 04 00 00 ce ff ff ff r4 += -50
       8:       bf 45 00 00 00 00 00 00 r5 = r4
       9:       0f 35 00 00 00 00 00 00 r5 += r3
      10:       0f 24 00 00 00 00 00 00 r4 += r2
      11:       71 44 00 00 00 00 00 00 r4 = *(u8 *)(r4 + 0)
      12:       71 55 00 00 00 00 00 00 r5 = *(u8 *)(r5 + 0)
      13:       5d 45 0a 00 00 00 00 00 if r5 != r4 goto +10 <LBB0_5>

0000000000000070 <LBB0_4>:
      14:       07 03 00 00 01 00 00 00 r3 += 1
      15:       bf 34 00 00 00 00 00 00 r4 = r3
      16:       67 04 00 00 03 00 00 00 r4 <<= 3
      17:       bf a5 00 00 00 00 00 00 r5 = r10
      18:       07 05 00 00 38 fe ff ff r5 += -456
      19:       0f 45 00 00 00 00 00 00 r5 += r4
      20:       07 02 00 00 01 00 00 00 r2 += 1
      21:       7b 25 00 00 00 00 00 00 *(u64 *)(r5 + 0) = r2
      22:       6d 31 ee ff 00 00 00 00 if r1 s> r3 goto -18 <LBB0_2>
      23:       05 00 06 00 00 00 00 00 goto +6 <LBB0_6>

00000000000000c0 <LBB0_5>:
      24:       67 02 00 00 03 00 00 00 r2 <<= 3
      25:       bf a4 00 00 00 00 00 00 r4 = r10
      26:       07 04 00 00 38 fe ff ff r4 += -456
      27:       0f 24 00 00 00 00 00 00 r4 += r2
      28:       79 42 00 00 00 00 00 00 r2 = *(u64 *)(r4 + 0)
      29:       6d 31 e7 ff 00 00 00 00 if r1 s> r3 goto -25 <LBB0_2>

00000000000000f0 <LBB0_6>:
      30:       b7 00 00 00 02 00 00 00 r0 = 2
      31:       95 00 00 00 00 00 00 00 exit
```

As for bytecode of kmp program, the line 5 is the condition of loop and the line 14 is the condition of if.  <LBB0_4> is the if branch and <LBB0_5> is the else branch. The program will return from rows 22 and 29 to the fifth row.  So we add assertion in line 5, 14, 22, 24  to describe the loop invariant.  The assertion will be like $\exists x_i. PROP(\forall 0 \leq i \leq r_3. -1 \leq *(SL - 456 + i * 8) \leq i )LOCAL(temp \ R_i \ r_i )SEP(r_i \mapsto x_i; Memory \ r_{10} \ SL)$

```ocaml
Definition KMP_STATEMENT : list Singleton_statement := ...

Definition Real_tag := (assertion_24, 24%nat) :: (assertion_22 , 22%nat) :: (assertion_14, 14%nat) :: (assertion_5 , 5%nat) :: Ebpf_ass_init.

Definition len := length (KMP_STATEMENT).

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) KMP_STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.
  
Definition Result := Program_exec Real_tag KMP_STATEMENT. 
```

The above is a concrete implementation in Coq. KMP_STATMENT represents the bytecode of kmp program. Real_tag means the assertions we write.  After_exec means the final assertions after we do several times symbolic execution. Need_to_check will generate the assertions we need to check the correctness. If you only care about passing, then you can get the result via Program_exec and ignore the intermediate calculation process. 

### Assertion for KMP

Here we take assertion_5 as an example.  Here as we do not need the Ebpf_context, we ignore it in our assertion.

```ocaml
Definition assertion_5 : assertion :=
  Aex 21%positive (Aex 22%positive (Aex 23%positive (Aex 24%positive
  (Anormal 
  (Up Pnot (Be Pvequal x (Ez_val 0)) :: (Qf PForall i (Bp Pimply (Bp Pand (Be Pvle (Ez_val 0%Z) i) (Be Pvle i (V_vari 21%positive))) (Bp Pand (Be Pvle (Ez_val (-1)) (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV)) (Be Pvle (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV) i) )) ) :: (Be Psvlt (V_vari 21%positive) x) :: nil)
    Ebpf_Local_init 
  (Data_at _ _ nullptr SCALAR_VALUE r0 :: Data_at _ _ x SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r5 :: Data_at _ _ nullptr NOT_INIT r6 :: Data_at _ _ nullptr NOT_INIT r7 :: Data_at _ _ nullptr NOT_INIT r8 :: Data_at _ _ nullptr NOT_INIT r9 :: Memory _ _ r10 Max_size SV :: nil)
  )))).
```

Here $Up,Be,Bp,Qf$ are the abbreviation for unary proposition operator, binary expression operator, binary proposition operator and quantifier 

(Up Pnot (Be Pvequal x (Ez_val 0)) means that $x \neq 0$ 

(Qf PForall i (Bp Pimply (Bp Pand (Be Pvle (Ez_val 0%Z) i) (Be Pvle i (V_vari 21%positive))) (Bp Pand (Be Pvle (Ez_val (-1)) (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV)) (Be Pvle (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV) i) )) ) means $\forall i , 0 \leq i \leq V_{21} \rightarrow -1 \leq SV[-456 + i << 3] \leq i$, here $V_{21}$ is the value stored at r4.

Here the value stored at r2,r3,r4,r5 are all existential variables as they have different values for different number of cycles.