::
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
+$  pod   $:(fin=term who=(set resource))
+$  pods  (map resource pod)
::
::  srkw - poke actions
::
::  %call-pod invites a (set ship) to join your pod.
::  %swimaway allows a ship to leave a pod.
::  %hearsong approves a request to join a pod.
::  %nearecho creates a self-hosted ring (ring between)
::    two or more chats that you own.
::
+$  srkw  $%  [%form-pod fin=term res=resource]
              [%call-pod =pod add=(set ship)]
              [%swimaway =pod]
            ::
              [%hearsong =pod luv=? rus=(unit resource)]
            ::
              [%nearecho fin=term rez=(set resource)]
              [%nearmore =pod rez=(set resource)]
          ==
::
::  sing - machine actions
::
::  %biggs sends a seal, or a new node for a pod.
::  %leave is an orca leaving one of your pods.
::
+$  sing  $%  [%biggs =pod res=resource seal=(map index node)]
              [%unite =pod]
              [%leave =pod]
          ==
::
::  wave - a graph-store "nodes" for an add-nodes poke
::
+$  wave  (map index node)
--