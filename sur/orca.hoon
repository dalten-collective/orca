::
::  Developer Notes
:: - Both parties need to do this - or it will be broken
:: - Check to confirm that it is the case that they've joined
:: - look @ how google docs simultaneous edits happen re: timing
::   this might help w/ clock skew
:: - Avoid infinite loop!
:: - Crow for stats
::
::  fertile reflection, roaming poetry
::
::
::  orca sur file
::
::  For my Father-in-Law. You must admit, this hoon is 
::  nice looking, no? Elegant, stylish and a touch of flair.
::  ... here and there.
::
::  Orcas are like that. lol.
::
/-  *post, *graph-store, *resource
::
|%
::
::  pod & pods - define a pod and associate a resource
::
+$  pod   [fin=term who=(set resource) eco=?]
+$  pods  (map resource pod)
+$  por   [=pod res=resource]
::
::  srkw - poke actions - (Southern Ring Killer Whales)
::
::  %call-pod invites a (set ship) to join your pod.
::  %swimaway allows a ship to leave a pod.
::  %hearsong approves a request to join a pod.
::  %nearecho creates a self-hosted ring (ring between)
::    two or more chats that you own.
::
+$  srkw  $%  [%form fin=term res=resource]
              [%call res=resource add=(set ship)]
              [%swim res=resource]
            ::
              [%hear urs=resource luv=? myn=resource]
            ::
              [%echo fin=term rez=(set resource)]
              [%more res=resource rez=(set resource)]
          ==
::
::  sing - machine actions
::
::  %biggs sends a seal, or a new node for a pod.
::  %leave is an orca leaving one of your pods.
::
+$  sing  $%  [%biggs =pod seal=wave]
              [%tempt =por]
              [%agree =por]
              [%leave res=resource]
              [%unite ser=(set resource)]
          ==
::
::  wave - a graph-store "nodes" for an add-nodes poke
::
+$  wave  (map index node)
::
::  kril - a list-unit of having performed ~(tap by wex.bowl)
::
+$  kril  [[wir=path sip=ship ter=@tas] [ack=? pat=path]]
--