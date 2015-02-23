Razor 0.9.0 (beta)
==================
Razor is a model-finder for first-order theories. A central guiding principle of Razor is that it can be accessible to users who lack expertise in formal methods. Application areas include software design, analysis of security protocols and policies, and configuration management. 
A core function of the tool is that it supports exploration of the space of models of a given input theory, as well as presents provenance information about the elements and facts of a model. The crucial mathematical tool is the ordering relation on models determined by homomorphism. Razor generates models that are minimal with respect to this homomorphism ordering.

# Installation
## Prerequisites
1. **Haskell and Cabal.** In order to build Razor, you need to install the latest version of the Haskell platform (https://www.haskell.org) and Cabal (https://www.haskell.org/cabal/) on your machine.
2. **Z3.** Razor utilizes an SMT solver for constructing models. In order to run the current version of Razor, you need to install Z3 (http://z3.codeplex.com) on your machine and make sure that `z3` is in the system path.

## Razor
1. Download Razor’s source code available at https://github.com/salmans/Razor to a local directory `RAZOR`.
2. Open a Terminal window and change directory to the local `RAZOR/src` directory:
<br> `cd RAZOR/src`
3. Install Razor using Cabal:
<br>`cabal install`

# Using the REPL

## Help
There is overall help (use `help`), as well as help for each specific mode (use `?`).

## Modes
There are several modes which logically separate the functions of the REPL into different categories. You will have to execute the respective commands (included below) to enter each mode. *Note:* In future releases, we plan to make the transition between modes automatic, so any command can be executed at any time.
- `@theory` Starting mode; turn the various knobs of Razor, and load an input theory.
- `@explore` Available once a satisfiable theory is loaded; explore the models of the given theory, and augment them with additional facts.
- `@explain` Available once a satisfiable theory is loaded; query the model's various provenance information for elements and facts.

## Command Examples

### Theory Mode
- `debug` Toggles debug mode
- `relax` Toggles relaxation of pure minimality; models produced in relaxed mode may contain collapses.
- `depth 5` Sets the Skolem depth to 5. By default the depth is -1, meaning unbounded search. 
- `load /path/to/razor/theory.raz` Loads the given Razor theory
- `tptp /path/to/tptp/problem.p` Loads the given TPTP problem

### Exploration Mode
- `current` Displays the current model
- `next` Get the next model available
- `aug R(e^2, e^100)` Augment the model with the given fact; for fresh elements not in the model, use elements not in the domain, like e^100
- `aug e^2=e^3` Augment the model with the equality of two elements in the model.
- `undo` Undo the previous augmentation; next cannot be undone. 

### Explanation Mode
- `origin e^0` Display a single origin of element e^0. That is, the instance of the theory sentence that caused this element to exist in the model. 
- `origins e^0` Display the possibly multiple origins of element e^0. 
- `origin* e^7` Display the single origin of element e^7; also, recursively display the origins of the elements that caused e^7 to exist, and soforth. 
- `origins* e^7` Display all origins of this element and it's recursive dependents.
- `blame R(e^2, e^3)` Display the sentence blamed for making R(e^2, e^3) true. That is, the instance of the theory sentence that caused the given fact to be true in the model. 

# Example Theory and Razor Interaction
## Lab Door Access Control Policy
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
user can explore the ```aug ~ r_3=r_2``` to see if any
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
