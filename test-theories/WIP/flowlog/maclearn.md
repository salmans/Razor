# maclearn.rzr
## SDNs and Flowlog.
SDNs aim to replace specialized legacy hardware with agnostic switches who communicate with one or more controllers. This redesign centralizes all network logic, making it possible for behavior to be specified in software, enabling more complex network applications. However, programming an SDN now requires three specific tasks: defining the network behavior, keeping track of the network state, and communicating with the switching hardware. Recent research has simplified this task into one, tier-less programming language called Flowlog \cite{nelson2014tierless}. Not only is this language advantageous to a developer, it is also a prime candidate for software analysis and verification techniques using a model finding assistant such as Razor. 
## Mac-Learning Switch.
Below is a Flowlog program for a traditional mac-address learning switch.
```SQL
TABLE learned(switchid, port, macaddr);
  ON packet_in(pkt):
    -- Learn port:mac pairs that are not already known
    INSERT (pkt.locSw, pkt.locPt, pkt.dlSrc) INTO learned WHERE
      NOT learned(pkt.locSw, pkt.locPt, pkt.dlSrc);
    -- Remove any outdated port:mac pairs
    DELETE (pkt.locSw, pt, pkt.dlSrc) FROM learned WHERE
      NOT pt = pkt.locPt;
    -- Unicast the packet when the destination is known
    DO forward(new) WHERE
      learned(pkt.locSw, new.locPt, pkt.dlDst);
    -- If the destination is not known, broadcast the packet
    DO forward(new) WHERE
      NOT learned(pkt.locSw, ANY, pkt.dlDst)
        AND NOT pkt.locPt = new.locPt;
```
Every Flowlog program is comprised of two sections: table declarations and declarative rules. These tables maintain a particular set of information as controller state. In this instance, the table keeps track of physical port to mac address (port:mac) pairs for a set of switches. The rules follow an SQL-like syntax, each of which will:

- Consume an incoming event (in this case an incoming packet)
- Check if the logical WHERE condition is met, which may reference current state
- If met, invoke a state change (insert/delete) and/or fire an outgoing event (forward)

## Flowlog Semantics
Incoming events are processed sequentially, each at a unique point in time. The processing of a single event can be thought of as a single time step. To capture this, we can augment each relation with a time element, and define the following axiom for any incoming event E: ```E(t, pkt) => exists st. succ(t)=st```. Note that this notion of time is quite simplistic. In situations where reach-ability of state must be calculated, a more expensive and complex notion of time would have to be specified. For our purposes this will suffice. Now that time is well defined, we must capture the persistence and mutation of state over time. The following axioms capture the semantics described in <a href="http://web.cs.wpi.edu/~tn/publications/nfsk-flowlog-nsdi14.pdf">the flowlog NSDI paper</a>. For any state table T:
```Haskell
T(t, x) & succ(t)=st => T(st,  x) | T-(t,  x)
T+(t, x) & succ(t)=st => T(st,  x)
T-(t, x) & T(t,  x) & ~T+(t,  x) & succ(t)=st => ~T(st,  x)
```
In english, these rules capture the fact that:

1. Information persists unless deleted.
2. Insertions will propagate.
3. Deletion is applied before insertion; that is, insertion overrides deletion.

The treatment of state and time in these rules follows closely to the efforts of incorporating time into Datalog, notably Dedalus. In a sense, the preceding definitions allow the flowlog rules to be "timeless." That is, any state change or outgoing event occurs at the same time as the incoming event. More importantly, reasoning about time is no longer necessary when translating the flowlog rules. The following is a translation of the mac learning switch program. The full geometric form translation is available in the .rzr file.
```Haskell
(1) packet(t, pkt) <=> learned+(t, locSw(pkt), locPt(pkt), dlSrc(pkt)) & 
        | learned(t, locSw(pkt), locPt(pkt), dlSrc(pkt))
        
(2) packet(t, pkt) & learned(t, locSw(pkt), pt, dlSrc(pkt)) <=> 
        learned-(t, locSw(pkt), pt, dlSrc(pkt)) | locPt(pkt)=pt
        
(3) packet(t, pkt) & learned(t, locSw(pkt), locPt(new), dlDst(pkt)) <=>
        forward(t, pkt, locPt(new))
        
(4) packet(t, pkt) <=> forward(t, pkt, locPt(new)) &
        | exists ANY. ~learned(t, locSw(pkt), ANY, dlDst(pkt)) 
        | locPt(pkt)=locPt(new)
```
## Systematic Exploration Through Augmentation.
Whoever wrote this program would probably like to explore the program's behavior step-by-step, as each packet is received. The user can start by supplying a starting packet event to Razor: ```exists pkt,t. packet(t, pkt)```. From one of the initial models, the user can now explore forward in time by selecting one of the two minimal instances and augmenting with ```aug packet(t_last, e_new)``` where t_last is the time step after the last packet event, and e_new is some new element. This enables the user to start from a small set of initial models, and walk through the space of models however they choose. For instance, if the user wanted to see a specific behavior trace of this program over five sequential packet events, the user could start with a single packet event, and augment four times from the models matching the partial trace they are interested in. This is similar to systematically traversing the conditional disjunctions of a given program, or how a developer may use a debugger. 
## Models for one packet event.
With the initial query, Razor returns two models. Both have a single packet event, in which the port:mac pair is learned in the next step. The difference is that in one model, this learned information is inserted (```learned+(...)```), while in the other model the information is learned previously (```learned(...)```). It is worth noting that the previously learned pair does not have an originating packet event, as our notion of time does not compute full back-traces to ensure a reachable state. The user can confirm that both of these models were generated from the first rule firing by explaining ```blame learned+(e)``` in the first model and ```blame learned(e)``` in the second model, where \( e}\) is the particular set of elements where both relations valuate to true in their respective models. Both questions return the same blamed sequent and grounded instance.
```Haskell
(1) packet(t_1,p_1) <=> learned+(t_1,sw_1,pt_1,dst_1)
        | learned(t_1,sw_1,pt_1,dst_1)
```
Curiously, none of the minimal models for one packet had the other three rules fire. The deletion rule did not execute as it requires another port:mac pair to be learned from some other packet event where the port was different and the mac address was the same. Since there was only one packet event, deletion cannot occur. Similarly, the two forwarding rules require another port:mac learned pair or just another port to send to. Since Razor returns minimal models, the existence of other ports or other learned pairs are extraneous with only one packet event occurring. Thus, no forwarding events were triggered.
##Provenance of Forwarding Events
Augmenting one of the initial models produces a plethora of models. The combination of insertion, deletion, previously learned information, and amount of disjunctions, results in a large number of permuted states. The most interesting of these models are the ones which trigger the forwarding rules; specifically, the models which forward both packets to different ports.
```Haskell
packet{(t_1,p_1), (t_2,p_2)}                            dlDst{(p_1) = dst_1, (p_2) = dst_2}
forward{(t_1,p_1,pt_2), (t_2,p_2,pt_1)}                 dlSrc{(p_1) = src_1, (p_2) = src_2}
learned-{(t_2,sw_1,pt_2,dst_1)}                         locPt{(p_1) = pt_1, (p_2) = pt_2}
learned{(t_1,sw_1,pt_1,src_1),                          locSw{(p_1) = sw_1, (p_2) = sw_2}
        (t_1,sw_1,pt_2,dst_1),(t_2,sw_1,pt_1,src_1),    succ{(t_1) = t_2, (t_2) = t_3}
        (t_2,sw_1,pt_2,dst_1),(t_2,sw_2,pt_1,dst_2),
        (t_2,sw_2,pt_2,src_2),(t_3,sw_1,pt_1,src_1),
        (t_3,sw_2,pt_1,dst_2),(t_3,sw_2,pt_2,src_2)}
```
While both packets have been forwarded, it is unclear why each was forwarded. The user is most likely curious if each packet was sent to a particular port, or broadcast out. Luckily, provenance information generated by the model finding assistant can eliminate this ambiguity. By explaining ```blame forward(t_1,p_1,pt_2)``` and ```blame forward(t_2,p_2,pt_1)```, Razor returns the following blamed sequents respectively.
```Haskell
(3) packet(t_1,p_1) & learned(t_1,sw_1,pt_2,dst_1) <=> forward(t_1,p_1,pt_2)  

(4) packet(t_2,p_2) <=> forward(t_2,p_2,pt_1)
        | exists ANY. learned(t_2,locSw(p_2),ANY,dlDst(p_2)
        | locPt(p_2) = pt_1
```
With provenance, it is now clear that the first packet was sent directly to the source port of the second packet, and that the second packet was broadcasted, because the destination has not been learned. In any flowlog program with multiple rules firing the same outgoing events, provenance information can elucidate the rule that caused the event to happen. 
