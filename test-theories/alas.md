# alas.rzr
This policy is for lab access involving a particular room, a couple of research groups, and their respective members. The user's intention for each policy rule follows.

```Haskell
(1) LabOf(`Logic,`TheLab);
(2) LabOf(`Systems, `TheLab);
(3) MemberOf(p,r) & LabOf(r,l) => Enter(p,l);
(4) HasKey(p,k) & KOpens(k,l) => Enter(p,l);
(5) COpens(cardOf(p),l) => Enter(p,l);
(6) Enter(p,l) => COpens(cardOf(p),l) | exists k. HasKey(p,k) & KOpens(k,l);
(7) COpens(cardOf(p), l) => exists r. MemberOf(p,r) & LabOf(r,l);
(8) HasKey(p,k) => exists e. Grant(e,p,k) & Employee(e);
(9) Grant(e,p,k)  => HasKey(p,k);
(10) MemberOf(p,`Systems) & HasKey(p,k) & KOpens(k,`TheLab) => Falsehood;
```

- (1-2) Logic and Systems are research groups belonging to the lab.
- (3-4) Members of a research group in a lab must be able to enter the lab.
- (4-5) Physical access via a key or a card allows a person to enter.
- (6) In order to enter a lab, you must have a valid key or card.
- (7) Cards are given to members of resident research groups.
- (8-9) Keys are granted to a person by an employee.
- (10) Systems research group members are not allowed to have keys.

Whoever is in charge of the lab would like to ensure that a thief could not possibly gain access to the lab under this policy. Using Razor, we can verify if there are any counterexamples: a thief can enter the lab, and is not a part of either resident research group. 

```Haskell
                          => Enter(`Thief,`TheLab)
MemberOf(`Thief,`Logic)   => Falsehood
MemberOf(`Thief,`Systems) => Falsehood
```
Razor returns two interesting scenarios in which the thief subverts the specified policy. 

##First Model: The thief is granted a key.
```Haskell
Enter{(p_1, l_1)}        			`TheLab=l_1
Employee{(e_1)}                  	`Systems=r_2
Grant{(e_1,p_1,k_1)} 		 		`Logic=r_1
HasKey{(p_1,k_1)}                	`Thief=p_1
KOpens{(k_1,l_1)}
LabOf{(r_1,l_1), (r_2,l_1)}
```
The first situation is rather easy to decipher: some employee granted
the thief a key to the lab. This is possible because the policy did
not restrict who can be granted a key. Moreover, there are no
restrictions over employee and their granting privileges. A curious
user may ask if the thief could have granted the key to themselves. As
this is extraneous information, the user must augment the current
model with this fact. They can do so by exploring ```aug p_1=e_1``` in the REPL. Applying the augmentation
produces one model; the user's curious suspicion is correct.

##Second Model: The thief is a member of a third research group.
```Haskell
Enter{(p_1, l_1)}        					`TheLab=l_1
COpens{(c_1,l_1)}     						`Systems=r_2
MemberOf{(e_1,r_3)}         				`Logic=r_1
LabOf{(r_1,l_1), (r_2,l_1),(r_3,l_1)} 		`Thief=p_1
											cardOf(p_1)=c_1
```
The second situation is more complex, and most likely raises several
questions in the user's mind. For instance, where did this third
research group come from? The user can look at the provenance of this
research group by explaining ```origin r_3``` in the
REPL. Razor pinpoints the causal sequent and it's grounded instance.
```Haskell
(7) COpens(c_1,l_1) => MemberOf(e_1,r_3) & LabOf(r_3,l_1)
```
This policy rule does not restrict which resident research group a
person must belong to in order for their card to open the lab. Such a
restriction would force the third research group to be equivalent to
either the Logic or Systems group. To confirm this policy fix, the
user can explore the ```aug r_3=r_2``` to see if any
other counter examples exist. Razor confirms that no possible breaches
occur from this augmented fix. The user may also be curious why the
thief has a card to begin with. By explaining ```blame COpens(c_1, l_1)```, the user can see the sequent blamed
for giving the user the card.
```Haskell
(6) Enter(p_1,l_1) => COpens(c_1,l_1) | HasKey(p_1,k_1) & KOpens(k_1,l_1)
```
As shown above, the thief has a card because it is one of the two
possible ways of accessing the lab. The other is via a key, which is
exactly the previously discussed model. At this point, the user may be
curious why the thief belongs to a research group in this scenario,
but not in the previous one. Being a research group member is only a
consequence of having a card, not for having a key. Belonging to a
research group when having a key is extraneous information, so Razor
does not include this scenario in the minimal models returned.
