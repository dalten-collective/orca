::
::  orca app file
::
::  For my Father-in-Law. Here, in this application, are 
::  the required elements to make shared songs between
::  many individuals.
::
::  May they grow harmoneously and may the melody of life
::  and The Word play for 10,000 years, to guide them.
::
/-  *orca, post, store=graph-store
/+  default-agent, dbug, *resource
::
|%
+$  versioned-state
    $%  state-zero
    ==
::
::  state-zero
::
::  fam - your currently federated resources, and their pods
::  pup - outgoing requests to increase your pod
::  cal - incoming requests to join a pod
::  mem - avoid infinite loop
::
+$  state-zero
  $:  %0
      fam=pods
      pup=(jug ship pod)
      cal=(set pod)
      mem=(set (map index node))
  ==
::
+$  card      card:agent:gall
+$  content   content:post
+$  eyre-id   @ta
--
::
%-  agent:dbug
=|  state-zero
=*  state  -
^-  agent:gall
=<
::!.
|_  =bowl:gall
+*  this  .
    fish  ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  ~&  >  [%orca %fluke %lift]
  :_  this
  ^-  (list card)
  :~  :*
    %pass   [%orca %spyhop ~]
    %agent  [our.bowl %graph-store]
    %watch  [%updates ~]
  ==  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  ole=vase
  ~&  >>  [%orca %fluke %wave]
  =/  old=versioned-state  !<(versioned-state ole)
  ?-    -.old
      %0
    =/  cards=(list card)
      :~  :*
        %pass   [%orca %spyhop ~]
        %agent  [our.bowl %graph-store]
        %watch  [%updates ~]
      ==  ==
    :_  this(state old)
    =/  wux  :-  [/orca/spyhop our.bowl %graph-store]
             [%.y /updates]
    ?:  (~(has in wex.bowl) wux)
      ~
   cards
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+    mark  `state
        %orca-song
      =/  vaz=sing  !<(sing vase)
      ?-    -.vaz
          %biggs
        :: add a new message to your version, also add
        :: to mem so you don't double-post it.
        `state
      ::
          %unite
        :: instruct everyone to listen
        `state
      ::
          %leave
        :: make sure to kick them from the subscription.
        `state
      ==
    ::
        %orca-dive
      =/  vaz=srkw  !<(srkw vase)
      ?-    -.vaz
          %form-pod
        =+  pud=[fin.vaz (sy ~[res.vaz])]
        =.  fam  (~(put by fam) [res.vaz pud])
        ~&  >  [%orca %form %pod fin.vaz]
        `state
      ::
          %call-pod
        `state
      ::
          %swimaway
        =+  podes=~(val by fam)
        ?~  (find ~[pod.vaz] podes)
          ~&  >>>  [%orca %spyhop %blank fin.pod.vaz]
          `state
        =+  caz=~(bye mom.fish pod.vaz)
        =.  fam  (~(rep by fam) ~(end mom.fish pod.vaz))
        ~&  >  [%orca %left %pod fin.pod.vaz]
        [caz state]
      ::
          %hearsong
        ?.  luv.vaz
          =.  cal  (~(del in cal) pod.vaz)
          `state
        ?~  rus.vaz
          ~&  >>>  [%orca %needs %bearing]
          `state
        =+  res=u.rus.vaz
        ?.  (team:title our.bowl entity.res)
          ~&  >>>  
            [%lobtail `@tas`(scot %p entity.res) name.res]
          `state
        ~&  >  [%follow %pod]
        =.  fam  (~(put by fam) res pod.vaz)
        :_  state
        ~(hey mom.fish pod.vaz)
      ::
          %nearecho
        =+  new=~(tap in (~(dif in rez.vaz) ~(key by fam)))
        ~&  >  [%orca %hears %itself `@tas`'-' fin.vaz]
        =+  pud=[fin.vaz rez.vaz]
        |-
        ?~  new
          `state
        $(new t.new, fam (~(put by fam) [i.new pud]))
      ::
          %nearmore
        =+  new=~(tap in (~(dif in rez.vaz) ~(key by fam)))
        ~&  >  [%orca %finds %more `@tas`'-' fin.pod.vaz]
        |-
        ?~  new
          `state
        $(new t.new, fam (~(put by fam) [i.new pod.vaz]))
      ==
    ==
  [cards this]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^  ^-  (quip card _this)
  ?+    -.sign  `this
      %kick
    ?+    wire  `this
        [%orca %spyhop ~]
      :_  this
      :~  :*
        %pass   [%orca %spyhop ~]
        %agent  [our.bowl %graph-store]
        %watch  [%updates ~]
      ==  ==
    ==
  ::
      %watch-ack
    ?~  p.sign
      `this
    ~&  >>>  [%nack wire]
    ~&  >>>  [%orca %panic]
    `this
  ::
      %fact
    ?+    +<.sign
      ~&  >>>  [%wat %mean]  `this
    ::
        %graph-update-3
      =/  act=action  +:!<(update +>.sign)
      ~&  >>  act
      ?+    -.act  `this
          %add-nodes
        ?.  (~(has in ~(key by fam)) resource.act)
          `this
          ::
        =/  kez=(list (list atom))
          ~(tap in ~(key by nodes.act))
        :_  this
        (~(sea mom.fish (~(got by fam) resource.act)) act)
      ==
    ==
  ==
  --  
::
++  on-watch  on-watch:def
::
++  on-arvo  on-arvo:def
::
++  on-peek  on-peek:def
::
++  on-leave  on-leave:def
::
++  on-fail  on-fail.def
--
::
|_  bol=bowl:gall
::
++  mom
  |_  p=pod
  ++  end
    |=  [in=[res=resource =pod] out=pods]
    ^-  pods
    ?:  =(p pod.in)
      out
    (~(put by out) res.in pod.in)
  ::
  ++  bye
    ^-  (list card)
    =+  [wav=~(tap in who.p) out=*(list card)]
    |-
    ?~  wav
      out
    %=    $
        wav
      t.wav
    ::
        out
      %+  welp  out
      :~  :*
        %pass   /loss/(scot %p our.bol)/(scot %tas fin.p)
        %agent  [entity.i.wav %orca]
        %poke   %orca-song  !>(`sing`[%leave p])
      ==  ==
    ==
  ::
  ++  hey
    ^-  (list card)
    =+  [wav=~(tap in who.p) out=*(list card)]
    |-
    ?~  wav
      out
    %=    $
        wav
      t.wav
    ::
        out
      %+  welp  out
      :~  :*
        %pass   /join/(scot %p our.bol)/(scot %p -.i.wav)
        %agent  [entity.i.wav %orca]
        %poke   %orca-song  !>(`sing`[%unite p])
      ==  ==
    ==
  ++  sea
    |=  act=action
    ^-  (list card)
    =+  [herd=~(tap in who.p) out=*(list card)]
    |-
    ?~  herd
      out
    ?:  &(=(our.bol entity.i.herd) ?=([%add-nodes *] act))
      =+  upd=[%add-nodes i.herd nodes.act]
      ~&  >>  "here"
      %=    $
          herd
        t.herd
      ::
          out
        %+  welp  out
        :~  :*
          %pass   /echo/(scot %da now.bol)/(scot %tas fin.p)
          %agent  [our.bol %graph-store]
          %poke   %graph-update-3  !>(`update:store`[now.bol upd])
        ==  ==
      ==
    %=    $
        herd
      t.herd
    ::
        out
      out
    ==
  --
--