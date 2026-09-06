# F14b Curation Checklist — Ambiguous GS LL Assignments

**Rows to classify:** 23 (10 ATP, 13 WTA)  
**Task per row:** Find the date the main-draw withdrawal was announced. Compare to qualifying-round completion (~2 days before the tournament date below). Record the source, withdrawal date, and update classification in `Data/cleaned/firstll/ll_lottery_classification.csv`.

**Classification rule:**

- `withdrawal_date <= qualifying_end` → `lottery`
- `withdrawal_date > qualifying_end` → `ranking`
- No date-able source found → `unresolved` (leave in the audit set)

---

## 1. Melle Van Gemerden — Roland Garros 2006 (ATP)

- **Player rank at event:** 111
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 3
- **Tournament date (Sackmann):** 2006-05-29

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2006_French_Open_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2006_French_Open_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20060529*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Melle%20Van%20Gemerden%22%20%22Roland%20Garros%22%202006%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202006)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Melle%20Van%20Gemerden%20withdraws%202006)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 2. Mariano Zabaleta — Roland Garros 2007 (ATP)

- **Player rank at event:** 88
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 4
- **Tournament date (Sackmann):** 2007-05-28

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2007_French_Open_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2007_French_Open_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20070528*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Mariano%20Zabaleta%22%20%22Roland%20Garros%22%202007%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202007)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Mariano%20Zabaleta%20withdraws%202007)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 3. Robin Haase — US Open 2007 (ATP)

- **Player rank at event:** 95
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2007-08-27

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2007_US_Open_(tennis)_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2007_US_Open_(tennis)_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (usopen.org)](https://web.archive.org/web/20070827*/usopen.org)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Robin%20Haase%22%20%22US%20Open%22%202007%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22US%20Open%22%202007)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Robin%20Haase%20withdraws%202007)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 4. Peter Luczak — US Open 2009 (ATP)

- **Player rank at event:** 78
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2009-08-31

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2009_US_Open_(tennis)_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2009_US_Open_(tennis)_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (usopen.org)](https://web.archive.org/web/20090831*/usopen.org)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Peter%20Luczak%22%20%22US%20Open%22%202009%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22US%20Open%22%202009)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Peter%20Luczak%20withdraws%202009)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 5. Martin Klizan — Australian Open 2014 (ATP)

- **Player rank at event:** 106
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2014-01-13

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2014_Australian_Open_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2014_Australian_Open_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (ausopen.com)](https://web.archive.org/web/20140113*/ausopen.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Martin%20Klizan%22%20%22Australian%20Open%22%202014%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Australian%20Open%22%202014)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Martin%20Klizan%20withdraws%202014)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 6. Kamil Majchrzak — US Open 2019 (ATP)

- **Player rank at event:** 94
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2019-08-26

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2019_US_Open_(tennis)_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2019_US_Open_(tennis)_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (usopen.org)](https://web.archive.org/web/20190826*/usopen.org)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Kamil%20Majchrzak%22%20%22US%20Open%22%202019%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22US%20Open%22%202019)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Kamil%20Majchrzak%20withdraws%202019)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 7. Brayden Schnur — Wimbledon 2019 (ATP)

- **Player rank at event:** 112
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 1
- **Tournament date (Sackmann):** 2019-07-01

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2019_Wimbledon_Championships_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2019_Wimbledon_Championships_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20190701*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Brayden%20Schnur%22%20%22Wimbledon%22%202019%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202019)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Brayden%20Schnur%20withdraws%202019)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 8. Francisco Cerundolo — Roland Garros 2021 (ATP)

- **Player rank at event:** 117
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2021-05-31

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2021_French_Open_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2021_French_Open_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20210531*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Francisco%20Cerundolo%22%20%22Roland%20Garros%22%202021%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202021)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Francisco%20Cerundolo%20withdraws%202021)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 9. Fabian Marozsan — Wimbledon 2023 (ATP)

- **Player rank at event:** 96
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 4
- **Tournament date (Sackmann):** 2023-07-03

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2023_Wimbledon_Championships_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2023_Wimbledon_Championships_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20230703*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Fabian%20Marozsan%22%20%22Wimbledon%22%202023%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202023)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Fabian%20Marozsan%20withdraws%202023)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 10. Giovanni Mpetshi Perricard — Wimbledon 2024 (ATP)

- **Player rank at event:** 58
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 5
- **Tournament date (Sackmann):** 2024-07-01

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2024_Wimbledon_Championships_%E2%80%93_Men%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2024_Wimbledon_Championships_%E2%80%93_Men%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20240701*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Giovanni%20Mpetshi%20Perricard%22%20%22Wimbledon%22%202024%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202024)
6. [ATP press-site search](https://www.atptour.com/en/search?query=Giovanni%20Mpetshi%20Perricard%20withdraws%202024)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 11. Nicole Pratt — US Open 2006 (WTA)

- **Player rank at event:** 79
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2006-08-28

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2006_US_Open_(tennis)_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2006_US_Open_(tennis)_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (usopen.org)](https://web.archive.org/web/20060828*/usopen.org)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Nicole%20Pratt%22%20%22US%20Open%22%202006%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22US%20Open%22%202006)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Nicole%20Pratt%20withdraws%202006)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 12. Julia Vakulenko — Wimbledon 2006 (WTA)

- **Player rank at event:** 90
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 1
- **Tournament date (Sackmann):** 2006-06-26

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2006_Wimbledon_Championships_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2006_Wimbledon_Championships_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20060626*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Julia%20Vakulenko%22%20%22Wimbledon%22%202006%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202006)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Julia%20Vakulenko%20withdraws%202006)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 13. Monica Niculescu — Roland Garros 2008 (WTA)

- **Player rank at event:** 86
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 1
- **Tournament date (Sackmann):** 2008-05-26

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2008_French_Open_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2008_French_Open_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20080526*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Monica%20Niculescu%22%20%22Roland%20Garros%22%202008%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202008)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Monica%20Niculescu%20withdraws%202008)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 14. Katie Obrien — Roland Garros 2009 (WTA)

- **Player rank at event:** 113
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2009-05-25

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2009_French_Open_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2009_French_Open_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20090525*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Katie%20Obrien%22%20%22Roland%20Garros%22%202009%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202009)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Katie%20Obrien%20withdraws%202009)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 15. Stephanie Dubois — Wimbledon 2010 (WTA)

- **Player rank at event:** 124
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 2
- **Tournament date (Sackmann):** 2010-06-21

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2010_Wimbledon_Championships_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2010_Wimbledon_Championships_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20100621*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Stephanie%20Dubois%22%20%22Wimbledon%22%202010%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202010)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Stephanie%20Dubois%20withdraws%202010)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 16. Misaki Doi — Wimbledon 2012 (WTA)

- **Player rank at event:** 105
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 1
- **Tournament date (Sackmann):** 2012-06-25

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2012_Wimbledon_Championships_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2012_Wimbledon_Championships_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20120625*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Misaki%20Doi%22%20%22Wimbledon%22%202012%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202012)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Misaki%20Doi%20withdraws%202012)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 17. Yulia Putintseva — Australian Open 2015 (WTA)

- **Player rank at event:** 114
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 1
- **Tournament date (Sackmann):** 2015-01-19

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2015_Australian_Open_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2015_Australian_Open_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (ausopen.com)](https://web.archive.org/web/20150119*/ausopen.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Yulia%20Putintseva%22%20%22Australian%20Open%22%202015%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Australian%20Open%22%202015)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Yulia%20Putintseva%20withdraws%202015)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 18. Mayar Sherif — US Open 2021 (WTA)

- **Player rank at event:** 96
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 6
- **Tournament date (Sackmann):** 2021-08-30

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2021_US_Open_(tennis)_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2021_US_Open_(tennis)_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (usopen.org)](https://web.archive.org/web/20210830*/usopen.org)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Mayar%20Sherif%22%20%22US%20Open%22%202021%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22US%20Open%22%202021)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Mayar%20Sherif%20withdraws%202021)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 19. Leolia Jeanjean — Australian Open 2023 (WTA)

- **Player rank at event:** 109
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 3
- **Tournament date (Sackmann):** 2023-01-16

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2023_Australian_Open_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2023_Australian_Open_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (ausopen.com)](https://web.archive.org/web/20230116*/ausopen.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Leolia%20Jeanjean%22%20%22Australian%20Open%22%202023%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Australian%20Open%22%202023)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Leolia%20Jeanjean%20withdraws%202023)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 20. Camila Osorio — Roland Garros 2023 (WTA)

- **Player rank at event:** 86
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 6
- **Tournament date (Sackmann):** 2023-05-29

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2023_French_Open_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2023_French_Open_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20230529*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Camila%20Osorio%22%20%22Roland%20Garros%22%202023%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202023)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Camila%20Osorio%20withdraws%202023)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 21. Yanina Wickmayer — US Open 2023 (WTA)

- **Player rank at event:** 85
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 3
- **Tournament date (Sackmann):** 2023-08-28

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2023_US_Open_(tennis)_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2023_US_Open_(tennis)_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (usopen.org)](https://web.archive.org/web/20230828*/usopen.org)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Yanina%20Wickmayer%22%20%22US%20Open%22%202023%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22US%20Open%22%202023)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Yanina%20Wickmayer%20withdraws%202023)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 22. Hailey Baptiste — Roland Garros 2024 (WTA)

- **Player rank at event:** 107
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 4
- **Tournament date (Sackmann):** 2024-05-27

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2024_French_Open_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2024_French_Open_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (rolandgarros.com)](https://web.archive.org/web/20240527*/rolandgarros.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Hailey%20Baptiste%22%20%22Roland%20Garros%22%202024%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Roland%20Garros%22%202024)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Hailey%20Baptiste%20withdraws%202024)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## 23. Renata Zarazua — Wimbledon 2024 (WTA)

- **Player rank at event:** 98
- **Rank among losers:** 1 (top-1: could be lottery OR ranking)
- **LL slots at event:** 4
- **Tournament date (Sackmann):** 2024-07-01

**Sources to check** (in order of expected yield):

1. [Wikipedia (main draw)](https://en.wikipedia.org/wiki/2024_Wimbledon_Championships_%E2%80%93_Women%27s_Singles)
2. [Wikipedia (qualifying)](https://en.wikipedia.org/wiki/2024_Wimbledon_Championships_%E2%80%93_Women%27s_Singles_Qualifying)
3. [Wayback Machine (wimbledon.com)](https://web.archive.org/web/20240701*/wimbledon.com)
4. [Google News (player + withdraws)](https://www.google.com/search?tbm=nws&q=%22Renata%20Zarazua%22%20%22Wimbledon%22%202024%20withdraws)
5. [Google News ("lucky loser" + tournament)](https://www.google.com/search?tbm=nws&q=%22lucky%20loser%22%20%22Wimbledon%22%202024)
6. [WTA press-site search](https://www.wtatennis.com/search?q=Renata%20Zarazua%20withdraws%202024)

**Fields to fill in the CSV:**

```
source:           <URL of the citing article>
withdrawal_date:  <YYYY-MM-DD>
withdrawn_player: <name, if identifiable>
classification:   lottery | ranking | unresolved
confidence:       high | medium | low
verifier_notes:   <one-line reason>
```

---

## After curation

Run `F14c_manual_review.R` (to be written) to:

1. Read the updated CSV back in
2. Re-count lottery-verified sample sizes
3. Re-estimate the headline stacked model on the expanded verified subsample
4. Regenerate `Tables_FirstLL/table_verified_firstll.tex`
5. Update the robustness section's verified-lottery numbers

