# Gforms navigation
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-001])
    title -.- Page1
    Page1 --> |1| Page2
    Page2 --> |2| Page3
    Page3 --> |3| Page4
    Page4 -.4 back.-> Page3
    Page3 -.5 back.-> Page2
    Page2 --> |6| Page3
    Page3 --> |7| Page4
    Page4 --> |8| Page5

    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle

```

***
***

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-002</u>])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 -.4 back.-> ATLPage1
    ATLPage1 -.5 back.-> Page1
    Page1 --> |6| ATLPage1
    ATLPage1 --> |7| ATLPage2

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle

```
***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-003</u><br><i>back button on CYA page <br>is missing])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLCYA11[ATL<br>Iteration1<br>CYA]

    ATLCYA11 -.5 back.-> ATLPage2


    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    class ATLCYA11 orangeCYA
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle

```
***
***

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-004</u>])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater["ATL<br>Iteration1<br>Repeater"]
    ATLRepeater --> |5| ATLPage12["ATL<br>Iteration2<br>Page1"]

    ATLPage12 --> |6| ATLPage22["ATL<br>Iteration2<br>Page2"]
    ATLPage22 -.7 back.-> ATLPage12
    ATLPage12 -.8 back.-> ATLRepeater

    ATLRepeater -.9 back.-> Page1
    Page1 --> |10| ATLRepeater

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle

```
***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-005</u>])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]
    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater["ATL<br>Iteration1<br>Repeater"]

    ATLRepeater -.5 back.-> Page1
    Page1 -.6 back.-> Page0
    Page0 --> |7| Page1
    Page1 --> |8| ATLRepeater

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle
```
***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-006</u>])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater["ATL<br>Iteration1<br>Repeater"]
    ATLRepeater --> |5| ATLPage12["ATL<br>Iteration2<br>Page1"]

    ATLPage12 --> |6| ATLPage22["ATL<br>Iteration2<br>Page2"]
    ATLPage22 --> |7| ATLRepeater2["ATL<br>Interation2<br>Repeater"]
    ATLRepeater2 -.8 back.->  Page1

    Page1 -.9 back.-> Page0
    Page0 --> |10| Page1
    Page1 --> |11| ATLRepeater2

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle

```
***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title(["<u>NavFLOW-007</u><br>current flow is broken<br>step 11 goes to Page4"])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]
    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater
    ATLRepeater --> |5| Page2
    Page2 --> |6| Page3
    Page3 --> |7|Page4
    Page4 -.8 back.-> Page3
    Page3 -.9 back.-> Page2
    Page2 -.10 back.-> ATLRepeater["ATL<br>Iteration1<br>Repeater"]

    ATLRepeater --> |11| Page2
    Page2 --> |12| Page3
    Page3 --> |13| Page4

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle

```
***
***

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-008</u><br>broken<br>step 6 goes to Page1])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater["ATL<br>Iteration1<br>Repeater"]
    ATLRepeater ===> |5 Change| ATLPage1
    ATLPage1 -.6 back.-> ATLRepeater

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle



```
***
***

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-009</u><br>broken<br>step 9 goes to Page1])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater["ATL<br>Iteration1<br>Repeater"]
    ATLRepeater --> |5| ATLPage12["ATL<br>Iteration2<br>Page1"]

    ATLPage12 --> |6| ATLPage22["ATL<br>Iteration2<br>Page2"]
    ATLPage22 --> |7| ATLRepeater2["ATL<br>Interation2<br>Repeater"]
    ATLRepeater2 ===> |8 Change| ATLPage1
    ATLPage1 -.9 back.-> ATLRepeater2


    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle
```
***
***

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-010</u>])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]

    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLRepeater["ATL<br>Iteration1<br>Repeater"]
    ATLRepeater --> |5| ATLPage12["ATL<br>Iteration2<br>Page1"]

    ATLPage12 --> |6| ATLPage22["ATL<br>Iteration2<br>Page2"]
    ATLPage22 --> |7| ATLRepeater2["ATL<br>Interation2<br>Repeater"]
    ATLRepeater2 ===> |8 Change| ATLPage12
    ATLPage12 -.9 back.-> ATLRepeater2


    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLPage2 orange
    class ATLPage1 orange
    class ATLPage12 orange
    class ATLPage22 orange
    class ATLRepeater2 orangeRepeater
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle
```

***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-011</u><br>back link is missing on CYA])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]
    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLCYA1["ATL<br>Iteration1<br>CYA"]
    ATLCYA1 --> |5| ATLRepeater["ATL<br>Iteration1<br>Repeater"]
    ATLRepeater ===> |6 Change|ATLCYA1
    ATLCYA1 -.7 back.-> ATLRepeater

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLCYA1 orangeCYA
    class ATLPage2 orange
    class ATLPage1 orange
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle


```
***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-012</u><br> no back link on CYA])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]
    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLCYA1["ATL<br>Iteration1<br>CYA"]
    ATLCYA1 --> |5| ATLRepeater["ATL<br>Iteration1<br>Repeater"]

    ATLRepeater ===> |6 Change|ATLCYA1
    ATLCYA1 ===> |7 Change| ATLPage1
    ATLPage1 -.8 back.-> ATLCYA1
    ATLCYA1 -.9 back.-> ATLRepeater

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLCYA1 orangeCYA
    class ATLPage2 orange
    class ATLPage1 orange
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle
```

***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-013</u>])
    title -.- Page0
    Page0 --> |1| Page1
    Page1 --> |2| ATLPage1["ATL<br>Iteration1<br>Page1"]
    ATLPage1 --> |3| ATLPage2["ATL<br>Iteration1<br>Page2"]
    ATLPage2 --> |4| ATLCYA1["ATL<br>Iteration1<br>CYA"]
    ATLCYA1 --> |5| ATLRepeater["ATL<br>Iteration1<br>Repeater"]

    ATLRepeater ===> |6 Change|ATLCYA1
    ATLCYA1 ===> |7 Change| ATLPage1
    ATLPage1 --> |8| ATLCYA1
    ATLCYA1 --> |9| ATLRepeater

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef orangeRepeater fill:#fe6e00,stroke:#222,stroke-width:3px
    class ATLRepeater orangeRepeater
    class ATLCYA1 orangeCYA
    class ATLPage2 orange
    class ATLPage1 orange
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle
```

***
***

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
flowchart TB
    title([<u>NavFLOW-014</u>])

subgraph top["change from CYA"]
    direction LR
    Page1 --> |1| Page2
    Page2 --> |2| Page3
    Page3 --> |3| Page4
    Page4 --> |4| Page5
    Page5 --> |5| CYA
    CYA   ===> |6 Change|Page2


end
subgraph scenario1["Scenario1: After changing Page2, all pages between Page2 and CYA are valid."]
    direction LR
    Page11["Page1"]
    Page21["Page2"]
    Page31["Page3<br><i>valid"]
    Page41["Page4<br><i>valid"]
    Page51["Page5"<br><i>valid]
    CYA1["CYA"]
    Page11 -.-  Page21
    Page21 -.- Page31
    Page31 -.-  Page41
    Page41 -.- Page51
    Page51 -.- CYA1
    CYA1   -.- Page21
    Page21 --> |7<br>fast forward| CYA1
end
subgraph scenario2["Scenario 2: After changing Page2, Page4 is invalid."]
    direction LR
    Page12["Page1"]
    Page22["Page2"]
    Page32["Page3<br><i>valid"]
    Page42["Page4<br><i>invalid"]
    Page52["Page5"<br><i>valid]
    CYA2["CYA"]
    Page12 -.-  Page22
    Page22 --> |7| Page32
    Page32 --> |8| Page42
    Page42 -.- Page52
    Page52 -.- CYA2
    CYA2   -.- Page22
    Page42 --> |9 <br>fast forward| CYA2
end
 title -.- top
 top --- scenario1
 scenario1 --- scenario2

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef valid fill:#1f6200,stroke:#222,stroke-width:3px
     classDef invalid fill:#ee6b6e,stroke:#222,stroke-width:3px
    class Page41 valid
    class Page31 valid
    class Page51 valid
    class Page32 valid
    class Page52 valid
    class Page42 invalid
    class CYA orange
    class CYA1 orange
    class CYA2 orange
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    class title greenTitle


```
***
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
flowchart TB
    title([<u>NavFLOW-015</u><br>Scenario2 is broken<br> step 7 goes to CYA ])

subgraph top["change from CYA"]
    direction LR
    Page1 --> |1| Page2["RevealingChoice"]
    Page2 --> |2| Page3
    Page3 --> |3| CYA

    CYA   ===> |5 Change|Page2


end
subgraph scenario1["Scenario1: After changing revealing choice, all pages are  valid."]
    direction LR
    Page11["Page1"]
    Page21["RevealingChoice"]
    Page31["Page3"]


    CYA1["CYA"]
    Page11 -.-  Page21
    Page21 -.- Page31
    Page31 -.-  CYA1
    CYA1   -.- Page21
    Page21 --> |6 <br>fast forward| CYA1
end
subgraph scenario2["Scenario 2: After changing revealing choice, Page4 is invalid."]
    direction LR
    Page12["Page1"]
    Page22["RevealingChoice"]
    Page32["Page3"]
    Page42["Page4<br><i>invalid"]

    CYA2["CYA"]
    Page12 -.-  Page22
    Page22 --> |6| Page32
    Page32 -.7 back.->Page22
    Page32 -.- Page42
    Page42 -.-  CYA2

    CYA2   -.- Page22

end
 title -.- top
 top --- scenario1
 scenario1 --- scenario2

    classDef orange fill:#fedebe,stroke:#222,stroke-width:1px
    classDef orangeCYA fill:#ffaf42,stroke:#222,stroke-width:2px
    classDef valid fill:#1f6200,stroke:#222,stroke-width:3px
     classDef invalid fill:#ee6b6e,stroke:#222,stroke-width:3px
    class Page41 valid
    class Page31 valid
    class Page51 valid
    class Page32 valid
    class Page52 valid
    class Page42 invalid
    class CYA orange
    class CYA1 orange
    class CYA2 orange
    classDef greenTitle fill:#a8ddb1 ,stroke:#c0c0c2,stroke-width:3px
    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px

    class title redTitle


```
***
```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph LR;
    title([<u>NavFLOW-016<br>Step 4 goes to Page2])
    Page2["Page2<br>with confirmation"]
    title -.- Page1
    Page1 -->  |1| Page2
    Page2 -.2 back.-> Page1
    Page1 --> |3 afer changing Page1| Page2
    Page2 --> |4 confirming with Yes| Page3

    classDef redTitle fill:#c21807 ,stroke:#c0c0c2,stroke-width:3px
    class title redTitle

```
# Notes for developers
Before updating the mermaid diagram in the file please test here:
https://mermaid.live/