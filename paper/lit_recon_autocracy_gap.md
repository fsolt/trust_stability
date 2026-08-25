# Literature Recon: The Autocracy Cell — Mass Attitudes → Regime-Change Events

<!-- Compiled 2026-08-24 (Claude-assisted, web-verified). Extends, does not repeat,
     `lit_review_regime_stability.md` (2026-07-21).
     Assigned thesis under test: "Cross-national tests linking mass political
     attitudes to regime-change EVENTS in AUTOCRACIES remain severely
     under-developed."
     Verification rule: every row traces to a source actually fetched or a
     search result actually returned. Items marked UNVERIFIED were seen only in
     secondary description and must be checked before citing.
     DV taxonomy used throughout:
       [EVENT]     = coded regime event (transition, breakdown, coup, episode onset)
       [SCORE]     = continuous democracy level/change (V-Dem, Polity, FH)
       [ATTITUDE]  = the DV is itself an opinion/attitude
       [BEHAVIOR]  = protest/campaign/mobilization events
-->

## Part 4. Why the autocracy cell is thin: the survey-data constraint

The paper's autocracy result rests on survey-measured discontent in regimes where
respondents have incentives to misreport. This is the single most likely line of
reviewer attack, and it is also the best explanation for why so few others have
run this test. The literature divides into (A) evidence that measured regime
support in autocracies is badly biased, (B) evidence that it is not, or not
everywhere, and (C) work on whether the *comparison* across regime types is
licensed at all.

### 4A. Evidence that measured regime support in autocracies is biased

| Study | Design & data | Sample | Headline result | Bearing on the paper |
|---|---|---|---|---|
| **Kuran 1991, *World Politics* 44(1) "Now Out of Never"; Kuran 1995/1997, *Private Truths, Public Lies* (Harvard UP)** | Theory of preference falsification and revolutionary cascades | — | Under repression, publicly expressed preferences decouple from private ones; regimes appear stable until a threshold cascade reveals accumulated opposition | Cuts **both ways**. It is the reason measured discontent in autocracies may be attenuated — and it is also the paper's own theoretical engine (cited at qmd L155, L186). If falsification is severe, observed discontent is a *floor* on true discontent, biasing coefficients toward zero, i.e. against the paper's own finding. |
| **Jiang & Yang 2016, *Comparative Political Studies* 49(5):600–634** — "Lying or Believing? Measuring Preference Falsification From a Political Purge in China" ⚠️ **NOT JLEO** — the brief's citation was wrong | Coincidence of the 2006 Shanghai purge (Chen Liangyu) with a nationwide survey in the field; synthetic measures of *expressed* vs. *actual* support, before/after | China, 2006 (+ two follow-up surveys over ~1 year) | Purge caused a sharp **rise in expressed** support alongside an equally clear **fall in actual** support — direct evidence of falsification. Strongest among those with alternative information but exposed to sanction. Falsification **did not persist**: political trust in Shanghai deteriorated substantially 6 months on | Establishes falsification is real and event-responsive. The decay result is helpful: falsification is a short-run distortion, less able to contaminate multi-year country-year series. |
| **Robinson & Tannenberg 2019, *Research & Politics* 6(3), DOI 10.1177/2053168019856449** (open access) | Four list experiments built on **standard comparative regime-support items** (the very items a DCPO-style pooling model would ingest), web survey | China | Systematic upward bias on **all four** items; direct questioning overstates support. Reported self-censorship in the published version ≈24.5–26.5% of respondents (working-paper version reported 16–22 pp). Highest among wealthy, urban, female, younger | The most directly damaging single study: it targets exactly the item pool used in cross-national regime-support scales, and explicitly warns that **cross-national studies comparing support across regime types may be systematically biased** if falsification is not uniform by regime type. |
| **Tannenberg 2022, *Democratization* 29(4):591–610, DOI 10.1080/13510347.2021.1981867** (open access) — "The autocratic bias: self-censorship of regime support" | Multilevel models; IV = respondent's *belief* that the government (not an independent institute) commissioned the survey; placebo comparison against apolitical items | **>228,000 respondents, 37 African countries** (Afrobarometer) | Fear of government produces substantial, significant bias on trust/approval/corruption items **in more autocratic countries but not in more democratic ones**; apolitical items unaffected. A V-Dem policy brief summarizes the inflation as "upward of 20 percentage points" | The comparability result, and therefore the most damaging for a *regime-interacted* design: the bias is conditional on regime type, so it is not absorbed by a country fixed effect or a constant offset. Note the Asianbarometer element in the brief is **not** part of this paper — it is Afrobarometer only. |
| **Carter, Carter & Schick 2024, *The China Quarterly* 259:804–813, DOI 10.1017/S0305741023001819** | Two online survey experiments, quota-sampled to 2010 census; direct questions vs. list experiments | China, ~2,000 respondents per wave, June & Nov 2020 (~4,000 total) | Direct questioning: >90% CCP support, 8% cite fear as a reason not to protest. List experiments: support falls to **50–70%**; ~40% acknowledge fear deters anti-regime protest. Authors call these **upper bounds** on true support | Recent, high-profile, and squarely on the "levels are uninterpretable" side. |
| **Kalinin 2016 (electoral support for an autocratic president upwardly biased)** — cited secondhand in Robinson & Tannenberg 2019 | List/indirect methods, Russia | Russia | Upward bias in measured electoral support | **UNVERIFIED** — seen only in secondary citation; verify outlet and exact claim before use. |

### 4B. Published counter-evidence: measured regime support in autocracies IS informative

This is the material the paper needs, and it is stronger than the "everything is
falsified" narrative implies.

| Study | Design & data | Sample | Headline result | Bearing on the paper |
|---|---|---|---|---|
| **Shen & Truex 2021, *British Journal of Political Science* 51(4):1672–1684** — "In Search of Self-Censorship" (note: title is *Self-Censorship*, not "Preference Falsification") | Self-Censorship Index = mean item-nonresponse on three regime-assessment items (confidence in government, in democracy, in human rights) minus mean nonresponse on three non-sensitive items (interpersonal trust, life satisfaction, confidence in TV) | WVS waves across ~100 countries / three decades; **59 authoritarian country-year samples**; plus the 2008 China Survey | **In most autocracies, citizens do NOT show higher item-nonresponse on regime-assessment questions than citizens of democracies.** Distribution is right-skewed: most autocratic samples cluster near zero. **China is the conspicuous outlier**, consistently high across the 2001, 2007 and 2012 WVS samples, with self-censorship concentrated among women, non-Party members and lower classes; the authors say this "raises doubts as to the reliability of measurement of political attitudes in China" | The single most useful defensive citation. It reframes the problem from "autocracy" to "a subset of autocracies, China above all," which is exactly the scope a 136-country panel can absorb. |
| **Frye, Gehlbach, Marquardt & Reuter 2017, *Post-Soviet Affairs* 33(1):1–15, DOI 10.1080/1060586X.2016.1144334** — "Is Putin's popularity real?" | List experiments with placebo tests, early 2015 | Russia | Indirect estimate of support ≈80%, **within 10 pp of direct questioning**; no evidence of positive bias from floor effects; placebos suggest a small *negative* bias (artificial deflation). Conclude approval ratings "largely reflect genuine attitudes" | Direct counter to the falsification-invalidates-everything claim, in a hard case. |
| **Frye, Gehlbach, Marquardt & Reuter 2023, *Post-Soviet Affairs* 39(3):213–222** — "Is Putin's popularity (still) real? A cautionary note on using list experiments…" | Repeat list experiments, late 2020–mid 2022, with placebo diagnostics | Russia | A naïve reading implies Putin is 10–20 points less popular than polls suggest, **but placebo experiments indicate those estimates are themselves artificially deflated** — i.e. the list-experiment corrective is not trustworthy either | Very useful: it undercuts the *evidentiary basis* of the bias literature, not just its conclusion. Cite when a reviewer treats list-experiment gaps as ground truth. |
| **Lei & Lu 2017, *Journal of Contemporary China* 26(104):213–232, DOI 10.1080/10670564.2016.1223104** ⚠️ **not *Political Behavior*** | Two randomized survey experiments varying administration procedure (incl. enumerator affiliation) | China (students; general public, 2014) | CCP-affiliated enumerators did **not** produce larger estimates of government support; later commentary notes respondents may even be *more* critical facing a CCP enumerator | Counter-evidence on the enumerator-fear channel. Caveat noted in the literature: fielded 2014, before the current repression peak. |
| **Shamaileh 2025, *Political Science Research and Methods* 13(2):373–391, DOI 10.1017/psrm.2024.29** — "On the measurement of preference falsification using nonresponse rates" | Formal argument + simulation + WVS | Cross-national | Nonresponse rates are **not** a valid proxy for falsification: as repression intensifies, respondents shift from "don't know" toward *pro-regime* answers, so states with identical SCI scores can have wildly different falsification levels; WVS shows little consistent relationship. Recommends freedom-of-expression measures instead as reliability proxies | Double-edged. It rebuts Shen & Truex's *reassuring* null as well as their method, so it weakens the best defensive citation — but it also implies the field has **no accepted instrument** for measuring falsification cross-nationally, which is itself an explanation for why the autocracy cell is empty. |
| **Mauk 2020, *Citizen Support for Democratic and Autocratic Regimes* (Oxford UP / ECPR series), ISBN 9780198854852** | Multilevel SEM on six pooled cross-national survey projects plus aggregate data | **>100 democracies and autocracies** | The **levels and individual-level sources of regime support are the same** in democracies and autocracies; what differs is how *system-level* context factors shape support. Explicitly framed as helping "assess the present and future stability of democracies and autocracies" | The strongest published statement that regime support is measurable and comparable across regime types. Also relevant to Part 5: this is a book-length treatment of support in autocracies whose stability payoff remains **theoretical** — the DV is an attitude, never an event. |
| **Tang 2016 (list experiment finds misreporting affects ~4% of respondents)** | List experiment, China | China | Very small misreporting | **UNVERIFIED** — seen only in secondary description (attributed as Tang 2016, likely *Populist Authoritarianism*, Oxford UP). Verify before citing. |

### 4C. Is the cross-regime comparison licensed? (measurement equivalence)

| Study | Design & data | Sample | Headline result |
|---|---|---|---|
| **Schneider 2017, *Social Indicators Research* 133(3):963–984, DOI 10.1007/s11205-016-1400-8** — "Can We Trust Measures of Political Trust? Assessing Measurement Equivalence in Diverse Regime Types" | Multiple-group CFA testing configural / metric / scalar invariance on a nine-item institutional-trust battery | Life in Transition Survey II (2010), ~39,000 households, **35 countries** (post-Soviet, Balkans, CEE, Western Europe) | Only the simplest four-indicator model achieves configural invariance across all 35 countries; **most models required excluding autocracies**. Several models reach partial *metric* invariance (correlates comparable) but fewer reach partial *scalar* invariance (means comparable). "Respondents in diverse cultures and regime types do not have equivalent understandings of political trust" on some parameters; clustering of measurement error differs between the democratic and autocratic parts of the sample |
| **Kirsch & Welzel (authoritarian notions of democracy)** — surfaced via *Frontiers in Political Science* 3:642283 (2021) invariance study | Measurement-invariance testing of liberal vs. authoritarian democracy notions, WVS | Cross-national | In most autocracies where measured support for democracy is high, what is supported is a **mix of liberal and authoritarian notions**, not democracy in the scholarly sense | 
| **Schedler 1999** (cited across this literature) | Conceptual | — | Doubts that reliable measures of regime legitimacy can be obtained in autocracies through representative surveys or interviews, given opacity and repression | **UNVERIFIED** as to exact outlet/page; it is a standard secondhand citation in this debate. |

### 4D. Assessment: how damaging is this to the paper?

**The honest reading is that the measurement critique is real, well-evidenced,
and survivable — but only if the paper meets it head-on.** Four points, in
descending order of usefulness:

1. **Direction of bias runs against the finding, not with it.** Every documented
   bias is *pro-regime* — respondents overstate support, i.e. **understate
   discontent**. Attenuation biases the discontent coefficient toward zero. A
   significant positive autocracy coefficient is therefore obtained *despite*
   the bias, and the true effect is plausibly larger. This is the paper's single
   strongest reply and it should be stated explicitly in the Data section or a
   robustness note.
2. **Levels vs. variance.** The bias literature is almost entirely about
   **levels** at a point in time (Robinson & Tannenberg, Carter et al., Tannenberg).
   The paper's estimand is **within- and across-country variation over 56 years**
   feeding a hazard model. A constant country-specific pro-regime offset is
   absorbed by the DCPO country-specific item-bias parameters and, in the hazard
   model, by anything that conditions on country. The threat is only from bias
   that *moves with the outcome* — e.g. falsification intensifying just before an
   episode onset. Jiang & Yang 2016 show falsification is event-responsive but
   **short-lived** (decayed within six months), which limits the plausible
   contamination of an annual series. This point is defensible but is currently
   **an argument the paper has not made anywhere in the qmd.**
3. **Tannenberg 2022 is the hard case and needs a direct answer.** Because the
   bias he documents is *conditional on regime type*, it does not net out of a
   design whose whole point is a democracy/autocracy interaction. A skeptic can
   argue the autocracy coefficient is partly a measurement artifact. The defense
   is again directional (bias suppresses measured discontent in autocracies, so
   the autocracy slope is if anything understated) plus the DCPO item-bias
   parameters — but the paper should name the study rather than leave it to a
   referee.
4. **The reassuring counter-evidence is genuine but contested.** Shen & Truex's
   near-null for most autocracies is the best defense; Shamaileh 2025 attacks
   its instrument. Frye et al. 2017 and 2023 are the more robust defensive pair,
   because 2023 shows that the list-experiment "correction" is itself unreliable
   in both directions. **No published work argues regime support in autocracies
   is flatly uninterpretable** — the strong claims are always item-, country-, or
   design-specific. Schedler's skepticism is the closest thing, and it is a
   conceptual caution, not a demonstration.

**Net:** the constraint explains the empty cell far better than it invalidates
the paper. The literature's own instruments for detecting falsification are in
open dispute (Shamaileh vs. Shen & Truex; Frye et al. 2023 vs. the list-experiment
canon), which is precisely the condition under which scholars avoid a research
design rather than one under which they have shown it to fail.

---

## Part 5. What actually exists on the autocracy side

Scope of the search: any study in which an **aggregate mass attitude** (not a
protest event, not an elite-coalition measure, not an expert-coded regime claim)
enters as a predictor of autocratic breakdown, autocratic transition, coup, or
autocratization. The search covered the legitimation/legitimacy literature, the
barometer literatures, the informational-autocracy literature, and the
autocratic-survival literature.

**The result is that the cell contains, at most, one study — and it reports a
null.** Everything else either has an attitude on the *left*-hand side, or has a
non-attitudinal IV, or restricts the sample to democracies.

### 5A. Attitudes as IV, regime instability as DV, autocracies in sample

| Study | IV (attitude) & measurement | DV & measurement | DV type | Sample | Result |
|---|---|---|---|---|---|
| **Claassen 2020, *AJPS* 64(1):118–134** (+ **Erratum 2024, *AJPS***, DOI 10.1111/ajps.12828) | Latent **democratic support**, Bayesian latent-variable model pooling cross-national survey items | Subsequent **change in V-Dem Liberal Democracy Index** (0–100) | **[SCORE]** | 135 countries, up to 29 years; **split-sample** into democracies and autocracies | Within democracies: positive, significant. **Within autocracies: insignificant, though positive.** The erratum reports the autocracy long-run effect as **4.40, 95% CI [−23.00, 25.23]** vs. 14.91 [7.59, 26.79] within democracies. Claassen's own summary: the evidence supports the democratic-survival hypothesis but **not** the autocratic-democratization hypothesis |
| **Tai, Hu & Solt 2024, *APSR* 118(1)** | Same latent support measure, with measurement uncertainty propagated | Same as Claassen 2020a | **[SCORE]** | Same | **Null throughout** — including, a fortiori, the autocracy side (already null before the correction) |
| **Welzel 2007, *IPSR* 28(4):397–424; Inglehart & Welzel 2005** | Emancipative / self-expression **values** (WVS aggregates) | Subsequent **level of democracy** (Freedom-House-based; "effective democracy") | **[SCORE]** | ~66 nations (Welzel); cross-national (I&W) | Positive and significant. But: level DV, no event coding, no regime-type interaction; autocracies enter only as low starting points |
| **Qi & Shin 2011, *IPSR* 32(3):245–262** | Prevalence of **"critical democrats"** (democratic support + low satisfaction) | Democratization / democratic development | **[SCORE]** | Cross-national | Positive. Same caveats |

That is the entire list. **No published study regresses a coded autocratic
regime-change EVENT (GWF autocratic breakdown, ERT episode onset, Powell–Thyne
coup, DD/BMR transition) on an aggregate mass-attitude measure.** This is the
same verdict Part 3 of the prior review reached from the DV side (none of 18
canonical regime-durability models include mass opinion); Part 5 confirms it
from the IV side.

### 5B. Autocracy-side studies where the attitude is the DEPENDENT variable

These are the studies most often mistaken for the missing test. Their common
structure is: something about the regime → citizens' attitudes. The stability
payoff is asserted in the framing, never estimated.

| Study | IV | DV | DV type | Sample | Result |
|---|---|---|---|---|---|
| **Guriev & Treisman 2020, *World Politics* 72(4):601–638, DOI 10.1017/s0043887120000167** | Repression, economic performance, information manipulation (media/internet censorship), political business cycle | **Leader approval** (Gallup World Poll) | **[ATTITUDE]** | Gallup World Poll, **>140 countries, 2006–2016** | Repression predicts *lower* approval in informational autocracies (and no significant repression–refusal relationship, which they read as evidence against preference falsification); economic performance and perceived public safety matter; covert censorship raises approval, but ratings fall when citizens detect it. **DV is approval, not survival.** |
| **Mauk 2020, *Citizen Support for Democratic and Autocratic Regimes* (Oxford UP)** | Individual- and system-level sources | **Regime support** | **[ATTITUDE]** | 6 pooled survey projects, >100 democracies and autocracies | Levels and individual-level sources of support are the same across regime types; system-level effects differ. Framed as informing "the present and future stability" of both regime types — but stability is never an estimated outcome |
| **Gilley 2006, *EJPR* 45:499–525 (measure); Gilley 2006, *IPSR* 27:47–71 (determinants); Gilley 2012, *EJPR* 51:693–699 (52-country update)** | Good governance, democratic rights, welfare gains | **State legitimacy index** (views of legality, system satisfaction, political violence, turnout, self-paid taxes) | **[ATTITUDE/index]** | 72 states c.2000 (83% of world population); 52 states c.2008 | Determinants identified; legitimacy **correlates** with the World Bank political-stability measure (r ≈ .51 unweighted, .57 weighted, .62 for the "justification" subtype) — a **cross-sectional correlation, not a hazard model**. Gilley himself flags reverse causation (stability generally treated as an effect, not a cause, of legitimacy) |
| **Magalhães, "Government effectiveness and support for democracy" (*EJPR*)** | Government effectiveness | **Support for democracy** | **[ATTITUDE]** | Cross-national, includes non-democracies | In contexts short of liberal democracy, effectiveness and democratic legitimacy are **negatively** related — relationships established in democracies do not travel. ⚠️ Volume/year **UNVERIFIED** (working PDF only) |
| **Dickson 2016, *The Dictator's Dilemma* (Oxford UP)** | Repression, propaganda, economic performance, controlled complaint channels | **Trust in and support for the CCP** | **[ATTITUDE]** | Nationwide Chinese surveys before and after the 2012 leadership transition | High measured support; concludes democratization is possible but unlikely. Single country, no event DV, and now a principal target of the Carter et al. 2024 measurement critique |

### 5C. Autocracy-side studies with a genuine EVENT DV — but a non-attitudinal IV

These are the papers whose *design* the present paper resembles. Every one of
them substitutes something observable for the attitude it actually wants.

| Study | IV & measurement | DV & measurement | DV type | Sample | Result |
|---|---|---|---|---|---|
| **Hollyer, Rosendorff & Vreeland 2015, *APSR* 109(4):764–784, DOI 10.1017/S0003055415000428** | **Transparency** = HRV Index of disclosure of 240 economic indicators to the World Bank WDI — an explicit *proxy* for citizens' shared information about regime performance | Autocratic regime removal via **mass revolt or democratization** (Svolik's authoritarian regime dataset); alt. DV = count of antigovernment demonstrations/strikes | **[EVENT]** | **Autocracies, 125 countries, 1980–2010**, regime-years | Cox competing-risks: a 1-SD rise in transparency raises the **autocratic collapse hazard by 40–50%**; raises protests/strikes but **not coups or assassinations** |
| **Hellmeier & Bernhard 2023, *CPS* 56(12):1858–1890, DOI 10.1177/00104140231152793** (open access) | Pro-democratic and pro-autocratic **mass mobilization**, new V-Dem expert-coded latent measures | Regime transformation / regime change; also quality of democracy | **[BEHAVIOR → EVENT + SCORE]** | ~179 polities, **1900–2021** | Pro-autocratic mobilization **accelerates democratic breakdown**. Authors note pro-autocratic mobilization data are scarcer than pro-democratic |
| **Knutsen, Dahlum, Rasmussen & Wig 2025/2026, *CPS*, DOI 10.1177/00104140251369338** (companion data paper: *BJPS* 55:e37, 2025) | **Support-coalition** size and social diversity — elite/group composition, not mass attitudes | Regime longevity / breakdown, incl. mode of breakdown | **[EVENT]** | Global, **1789–2020** (>230 years) | Larger and more diverse coalitions → longer regime survival; holds in autocracy-only subsamples; larger coalitions limit **popular uprisings** but not coups |
| **"Strategies of political control and regime survival in autocracies," *Democratization* (2025), DOI 10.1080/13510347.2025.2611256** | Six repression / co-optation / **indoctrination** strategies (V-Dem + Varieties of Indoctrination) | Autocratic regime **breakdown** | **[EVENT]** | **229 regimes, 1946–2010** | Media indoctrination and repression of civil-liberty rights outperform other strategies in predicting breakdown. **Indoctrination is the closest published stand-in for "what citizens believe" — and it is measured on the regime's side, not the citizens'.** ⚠️ Author names not captured; verify before citing |
| **Grauvogel & von Soest 2014, *EJPR* 53(4):635–653** | **Legitimation claims** (expert-coded, 98 states 1991–2010) — regime claims, not citizen beliefs | Democratization under sanctions | **[EVENT-ish]** | Global sanctions dataset 1990–2011 | **fsQCA**, not regression: sanctions strengthen authoritarian rule when the regime folds them into its legitimation strategy |

### 5D. The legitimation literature's own explanation of why it works this way

The autocratic-legitimation programme is large (Gerschewski 2013 alone has ~726
citations), explicitly names citizen legitimacy beliefs as a pillar of autocratic
stability — and then **deliberately declines to measure them with surveys.**

- **Gerschewski 2013, *Democratization* 20(1):13–38** — the three-pillars framework
  (legitimation, repression, co-optation), motivated by the observation that
  research had "gradually lost sight of the legitimation dimension." Treats
  measurement as an open problem and concedes that for some components
  measurement still depends on qualitative country-expert assessment, with no
  suitable indicator available for larger studies.
- **Gerschewski 2018, *Perspectives on Politics* 16(3):652–665**, "Legitimacy in
  Autocracies: Oxymoron or Essential Feature?" — programmatic article whose
  stated aims include "tackling concrete challenges in measuring this elusive
  concept," and which asks explicitly "how one can assess the degree to which a
  non-democracy is viewed as legitimate by its citizens." Later reworked as ch. 2
  of *The Two Logics of Autocratic Rule* (Cambridge UP, 2023). *(Only abstract
  and metadata retrieved; do not attribute in-text quotations without the PDF.)*
- **Dukalskis & Gerschewski 2017, *Contemporary Politics* 23(3):251–268** — four
  mechanisms of autocratic legitimation (indoctrination, passivity, performance,
  democratic-procedural). Conceptual typology; the "what citizens hear" half is
  theorized, not measured.
- **von Soest & Grauvogel 2017, *Contemporary Politics* 23(3)**; **Grauvogel &
  von Soest, "Legitimacy and Legitimation Strategies in Authoritarian Regimes,"
  in Wolf (ed.), *The Oxford Handbook of Authoritarian Politics* (2024)** — they
  separate **legitimation** (the regime's claim-making) from **legitimacy**
  (citizens' perceptions), and justify studying only the former on the grounds
  that, given autocracies' opaque and repressive nature, their legitimacy is
  **notoriously difficult to measure with public opinion surveys**.
  ⭐ The single most useful quotable acknowledgement found: leading scholars of
  autocratic legitimacy say they study regime *claims* precisely because citizen
  *attitudes* cannot be measured well in autocracies.
- **Tannenberg, Bernhard, Gerschewski, Lührmann & von Soest 2019, V-Dem WP 86**,
  published as **"Claiming the right to rule: regime legitimation strategies from
  1900 to 2019," *European Political Science Review* 13(1):77–94 (2021), DOI
  10.1017/S1755773920000363** — expert-coded legitimation claims for **179
  countries, 1900–2018**, now a V-Dem module (commissioned by the World Bank).
  Explicitly positioned as the alternative to the survey route: "Unlike much of
  the existing literature, which focuses on regime acceptance via representative
  surveys, this work analyzes the different bases on which democratic and
  nondemocratic regimes claim legitimacy." A measurement paper; **no
  regime-survival estimation.**

### 5E. Pattern

The autocracy literature has three moves available and uses all of them **except**
the one this paper makes:

1. **Measure the regime's claims instead of citizens' beliefs** (RLS/V-Dem,
   von Soest & Grauvogel, Varieties of Indoctrination).
2. **Measure citizens' behavior instead of their attitudes** (Hellmeier &
   Bernhard; and the whole protest/campaign tradition catalogued in Part 1C of
   the prior review).
3. **Measure the attitude, but make it the outcome** (Guriev & Treisman, Mauk,
   Gilley, Dickson).

The fourth cell — measure the attitude, put it on the right-hand side, and put a
coded regime EVENT on the left, in autocracies, cross-nationally — is the one the
present paper occupies.

---

## Part 6. The asymmetry: the political-support literature is about democracies

### 6A. Documented scope restrictions (strongest evidence first)

| Source | Nature of the restriction | Evidence |
|---|---|---|
| ⭐ **Valgarðsson, Jennings, Stoker, Bunting, Devine, McKay & Klassen 2025, *BJPS* 55:e15, DOI 10.1017/S0007123424000498** — "A Crisis of Political Trust? Global Trends in Institutional Trust from 1958 to 2019" | The current state-of-the-art global institutional-trust series — **3,377 country-year-surveys, 5.3 million respondents, 50 survey projects, 143 countries, 1958–2019**, six institutions, Bayesian dynamic latent trait models — **explicitly excludes autocracies from its regional and global trend analysis**, restricting those analyses to countries coded "liberal democracy" or "electoral democracy" in a majority of years. Individual country trends cover all 143. It tests **no** regime-stability consequence, noting only that declining trust "may nevertheless, directly or indirectly, threaten the future of democratic governance" | The cleanest possible demonstration: the field's most comprehensive global trust data are *deliberately* analysed democracies-only |
| ⭐ **Grauvogel & von Soest, "Legitimacy and Legitimation Strategies in Authoritarian Regimes," in Wolf (ed.), *Oxford Handbook of Authoritarian Politics* (2024)** | Justify studying regime *claims* rather than citizen *beliefs* because, given autocracies' opaque and repressive nature, autocratic legitimacy is "notoriously difficult to measure with public opinion surveys" | An explicit, authoritative statement that the survey route into autocratic legitimacy is treated as closed |
| ⭐ **Tannenberg, Bernhard, Gerschewski, Lührmann & von Soest, *EPSR* 13(1):77–94 (2021)** | "Unlike much of the existing literature, which focuses on regime acceptance via representative surveys, this work analyzes the different bases on which democratic and nondemocratic regimes claim legitimacy" | The field's turn *away* from surveys in the autocracy cell, stated as a design choice |
| **Magalhães, "Government effectiveness and support for democracy" (*EJPR*)** | "The usual practice in the study of popular support for regimes — to focus only on what occurs within democracies — may be neglecting the investigation of the sources of popular support for dictatorships (Geddes & Zaller 1989)"; and finds effectiveness relates to democratic legitimacy **negatively** outside liberal democracies, i.e. democracy-derived relationships do not travel | ⚠️ Quote is from a working-paper PDF that could not be text-extracted; **verify wording, volume and year before quoting** |
| **Mauk, "Regime support and its sources in democracies and autocracies"** (conference paper; developed into Mauk 2020, Oxford UP) | "With the bulk of political-culture research concentrating on democracies, political attitudes and their generation in autocratic regimes have received only limited theoretical or empirical attention" | ⚠️ Surfaced as a 2017 draft marked "do not cite" — **cite the 2020 book instead** and locate the equivalent passage there |
| **Katz & Levin, "Varieties of Political Support in Emerging Democracies: A Cross-National Analysis," *Social Science Research*** | "Because much of this pioneering literature focused on established democracies, where adherence to the democratic ideal is nearly universal (Klingemann 1999; Norris 1999)," certain support types "have been subject to less theoretical development" | ⚠️ Volume/year/pages **UNVERIFIED**; note the scope is *emerging democracies*, still not autocracies |
| **Dalton 2004, *Democratic Challenges, Democratic Choices: The Erosion of Political Support in **Advanced Industrial Democracies*** (Oxford UP); Dalton's chapter in Norris (ed.) 1999 is "Political Support in **Advanced Industrial Democracies**"; Newton's is "Social and Political Trust in **Established Democracies**" | Scope declared in the titles themselves | Structural, not a quotation — but unambiguous |
| **Norris 1999 *Critical Citizens* (Oxford UP) / 2011 *Democratic Deficit* (Cambridge UP)** | Norris widens coverage to 50+ societies in 2011, but the organising construct — the "democratic deficit," the gap between democratic aspiration and perceived democratic performance — **presupposes a democratic referent** | No explicit authorial statement excluding autocracies was located in the retrieved material. **Do not attribute one.** |

### 6B. The backsliding/deconsolidation literature has the same shape

- **Waldner & Lust 2018, *ARPS* 21:93–113, DOI 10.1146/annurev-polisci-050517-114628** — the field's standard review of democratic backsliding. Conceptually it defines backsliding as change *within* a regime, i.e. it starts from a democracy. It is blunt that the evidence base is weak: key hypotheses "lack a large body of confirming cross-national evidence"; the correlation among five backsliding indicators is only 0.33 (vs. 0.85–0.86 for regime type); robustness checks of three studies in leading journals held up in only 25% of cases for one and under 75% for two others, at the low bar of correct sign and p<0.10. The retrieved material foregrounds structural and elite-level explanations; **no treatment of mass attitudes as a driver was located** — verify against the full PDF before claiming its absence in print.
- **Boese, Edgell, Hellmeier, Maerz & Lindberg 2021, *Democratization* 28(5):885–907, DOI 10.1080/13510347.2021.1891413** (open access) — democratic resilience as a two-stage process (avoiding autocratization onset; averting breakdown once underway), using ERT. Democracies-only by construction; once autocratization begins, only about one in five democracies avert breakdown. Predictors are judicial constraints and historical institutional strength, **not attitudes**.
- **Jacob 2025, *EJPR* 64(1):348–373, DOI 10.1111/1475-6765.12703** — the most sophisticated recent opinion-and-regime-trajectory paper. IV: anti-pluralist parties in government (V-Party × seat share) interacted with **citizen support for democracy** (Bayesian dynamic latent trait model over 4.7 million responses, 145 countries). DV: **annual downturn in V-Dem Liberal Democracy Index** [SCORE], negative changes only. Sample: **~100 democracies, 1990–2019**. Anti-pluralist government × weak democratic support significantly predicts backsliding; **null** for support → subsequent anti-pluralist rhetoric and for rhetoric → support. **Autocracies are outside the sample by construction.**
- **Foa & Mounk 2016/2017 and the rebuttal exchange** (covered in Part 2 of the prior review) — US and Western Europe throughout.
- **Wuttke, Gavras & Schoen 2022, *BJPS* 52(1)** — 18 **consolidated** democracies.

### 6C. The autocracy-side attitude literature is single-country or non-event

Where mass attitudes in autocracies *are* studied seriously, the work is
overwhelmingly case-based, and the stability outcome is theorized rather than
estimated.

| Study | What it is | Why it is not the missing test |
|---|---|---|
| **Neundorf, Ozturk, Northmore-Ball, Tertytchnaya & Gerschewski 2025, *CPS* 58(9):1854–1889, DOI 10.1177/00104140241283006** (open access) — "A Loyal Base: Support for Authoritarian Regimes in Times of Crisis" | Distinguishes **normative** support (strongman leadership, patriotism, leader attachment) from **instrumental** performance-based support; two original online surveys plus observational data | **Turkey only.** DV is voter defection in crisis, not a regime event. Explicitly framed as having implications "for research on mass opinion and defection cascades in electoral autocracies" — i.e. it names the macro question it does not test |
| **Matovski 2021, *Popular Dictatorships: Crises, Mass Opinion, and the Rise of Electoral Authoritarianism* (Cambridge UP), 316pp.** | Argues electoral autocracies succeed through genuine mass appeal generated by crisis-driven demand for order, not superior repression or manipulation | Includes a macro chapter on "the crisis roots of these regimes," but the core evidence is comparative-historical plus a Russia case study; the DV is regime **emergence and appeal**, not a hazard of breakdown |
| **Dickson 2016; Chen, Zhong & Hillard 1997; Zhong & Chen 2013; Lü 2014, *APSR* 108(2)** | The China regime-support tradition (Eastonian diffuse support is explicitly the frame in Chen et al. 1997) | Single country; DV is the attitude. ⚠️ Citations for the Chen/Zhong/Lü line are **UNVERIFIED** (secondary description only) |

**Net for Part 3 of the brief:** the asymmetry is real and, in two cases,
*documented in the literature's own words* — the leading global trust series
analyses democracies only, and the leading autocratic-legitimacy scholars say in
print that they avoid surveys because autocratic legitimacy cannot be measured
well with them. That is a stronger foundation than a bare count of studies.

---

## Part 7. Recency check, 2023–2026: has anyone moved into the cell?

Search strategy: ERT/episode-onset + opinion; latent public-opinion measures +
autocratic breakdown; regime support + coups/breakdown; V-Dem working papers;
2025–2026 journal output. **Nothing found that occupies the cell.** The nearest
approaches, ranked by how much they threaten the novelty claim:

| Study | Why it is close | Why it does not scoop the paper | Threat |
|---|---|---|---|
| **Sato & Wiebrecht 2024, *Political Research Quarterly* 77(3):1010–1025, DOI 10.1177/10659129241252811** — "Disinformation and Regime Survival" (also V-Dem WP 144, with Lindberg, SSRN 4569010) | **The design analogue.** DV = **ERT episode-onset binary** (first year of a democratization or autocratization episode); regime-type-split analysis; probit with country and year FE, clustered SEs, plus an IV strategy; **148 countries, 2000–2022** (1,314 autocracy and 1,454 democracy observations). Results: in autocracies disinformation **reduces** democratization onset (−0.519, p<.01); in democracies it **raises** autocratization onset (0.731, p<.01) | IV is **expert-coded government disinformation** (Digital Society Project, Bayesian IRT), not mass attitudes. Mass opinion enters only as a proposed mechanism. Coverage 2000–2022 vs. the paper's 1968–2023 | **Moderate.** Establishes that ERT-onset × regime-type is now a published design, so the paper's novelty must rest squarely on the **IV**, not the DV or the interaction. Cite it — a reviewer familiar with this literature will expect it |
| **Jacob 2025, *EJPR* 64(1):348–373** | Latent democratic support (145 countries) as a conditioning variable on regime trajectories | DV is a **score** (LDI downturn), sample is **democracies only**, and support enters interactively with party anti-pluralism | **Low**, but it is the state of the art on the democracy side and should be cited as such |
| **Neundorf et al. 2025, *CPS* 58(9)** | Regime support in an autocracy, with defection cascades as the stated stake | Turkey only; DV is vote defection | **Low** |
| **Knutsen, Dahlum, Rasmussen & Wig 2025/26, *CPS*, DOI 10.1177/00104140251369338** | Regime "support" coalitions → survival, autocracy subsamples, mode-of-breakdown extensions | "Support" here is **elite/group coalition composition**, not mass attitudes | **Low**, but the title invites confusion — distinguish explicitly |
| **"Strategies of political control and regime survival in autocracies," *Democratization* (2025), DOI 10.1080/13510347.2025.2611256** | Indoctrination + repression → autocratic breakdown, 229 regimes 1946–2010 | Regime-side strategies, not citizen attitudes | **Low** |
| **Baker, Eltokhy & Guo 2026, "Empirical Global Games of Regime Change," arXiv:2607.05534 (6 July 2026)** | Structural estimation of a global game of regime change; DV = coup occurrence and success; decomposes outcomes into "feasibility" and "desirability" | Uses **observable fundamentals**, not survey-measured beliefs. Preprint, not peer-reviewed | **Low**, but monitor — if a revision adds survey measures of discontent it would become directly competing |
| **Shamaileh 2025, *PSRM* 13(2):373–391** | Newest word on measuring falsification | Methodological; no regime outcomes | **None** (but see 4B — it cuts against the best defensive citation) |
| **Ma, Choi, Tai, Hu & Solt 2026, *EPSR* (FirstView, 12 Aug 2026), DOI 10.1017/S1755773926100599** — "Macrodiscontent across countries" | **The paper's own measurement companion**: PPD latent estimates, 136 countries, 1968–2023, 111 items | Measurement + correlates of discontent (discontent has risen in OECD democracies, driven mainly by economic conditions); **does not test regime-stability consequences** | **None** — same team. But note the qmd currently cites this as `@Ma_Choi_Tai_Hu_Solt_2026` with no journal; **update the bib entry to EPSR with the DOI above** |

**No scoop found.** The one design that has arrived in print (Sato & Wiebrecht)
uses the paper's DV apparatus with a regime-side IV, which if anything makes the
paper's contribution easier to state: the ERT-onset hazard framework is
established; what has never been put on its right-hand side is a measure of what
citizens actually think.

---

## Sources

**Part 4 — measurement constraint**
- Robinson & Tannenberg 2019, *Research & Politics*: https://journals.sagepub.com/doi/full/10.1177/2053168019856449 (DOI 10.1177/2053168019856449); working paper https://doi.org/10.2139/ssrn.3161915 ; open PDF https://www.diva-portal.org/smash/get/diva2:1343477/FULLTEXT01.pdf
- Tannenberg 2022, *Democratization* 29(4):591–610: https://www.tandfonline.com/doi/full/10.1080/13510347.2021.1981867 ; Afrobarometer WP 176 version: https://www.afrobarometer.org/wp-content/uploads/2022/02/afropaperno176_autocratic_trust_bias.pdf
- Jiang & Yang 2016, *CPS* 49(5):600–634: https://journals.sagepub.com/doi/10.1177/0010414015626450 ; author PDF http://www.junyanjiang.com/uploads/5/8/1/9/58193547/comparative_political_studies-2016-jiang-600-34.pdf
- Shen & Truex 2021, *BJPS* 51(4):1672–1684: https://www.cambridge.org/core/journals/british-journal-of-political-science/article/in-search-of-selfcensorship/BC47657718E97A0BBED98CC391F06F88
- Shamaileh 2025, *PSRM* 13(2):373–391: https://www.cambridge.org/core/journals/political-science-research-and-methods/article/on-the-measurement-of-preference-falsification-using-nonresponse-rates/21FBAD9B277109F189012A5EABD0CCFB (DOI 10.1017/psrm.2024.29)
- Carter, Carter & Schick 2024, *China Quarterly* 259:804–813: https://www.cambridge.org/core/journals/china-quarterly/article/do-chinese-citizens-conceal-opposition-to-the-ccp-in-surveys-evidence-from-two-experiments/12A2440F948D016E8D845C492F7D0CFE (DOI 10.1017/S0305741023001819)
- Frye, Gehlbach, Marquardt & Reuter 2017, *Post-Soviet Affairs* 33(1):1–15: https://www.tandfonline.com/doi/abs/10.1080/1060586X.2016.1144334 ; PDF https://www.almendron.com/tribuna/wp-content/uploads/2017/02/Is-Putin-s-popularity-real.pdf
- Frye et al. 2023, *Post-Soviet Affairs* 39(3):213–222: https://www.tandfonline.com/doi/full/10.1080/1060586X.2023.2187195
- Lei & Lu 2017, *Journal of Contemporary China* 26(104):213–232: https://doi.org/10.1080/10670564.2016.1223104
- Schneider 2017, *Social Indicators Research* 133(3):963–984: https://pmc.ncbi.nlm.nih.gov/articles/PMC5579303/ (DOI 10.1007/s11205-016-1400-8)
- Measurement invariance / authoritarian notions of democracy: https://www.frontiersin.org/articles/10.3389/fpos.2021.642283/full
- Mauk 2020 (Oxford UP): https://global.oup.com/academic/product/citizen-support-for-democratic-and-autocratic-regimes-9780198854852 ; ECPR summary https://ecpr.eu/news/news/details/605

**Part 5 — autocracy side**
- Claassen 2020, *AJPS* 64(1):118–134: https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12452 ; erratum https://onlinelibrary.wiley.com/doi/10.1111/ajps.12828 ; accepted version https://eprints.gla.ac.uk/183785/7/183785.pdf
- Guriev & Treisman 2020, *World Politics* 72(4):601–638: https://www.cambridge.org/core/journals/world-politics/article/abs/popularity-of-authoritarian-leaders/3EB2352F226F8904DBB0293A83F10622 ; PDF https://www.danieltreisman.org/s/popularity_of_authoritarian_leaders-as-published.pdf
- Gilley 2006 *EJPR* 45:499–525: https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1475-6765.2006.00307.x ; PDF https://web.pdx.edu/~gilleyb/MeaningMeasureofLegitimacyFor72.pdf ; Gilley 2006 *IPSR* 27:47–71 https://journals.sagepub.com/doi/10.1177/0192512106058634 ; 52-country update https://web.pdx.edu/~gilleyb/StateLegitimacyUpdate52.pdf
- Hollyer, Rosendorff & Vreeland 2015, *APSR* 109(4):764–784: https://www.cambridge.org/core/journals/american-political-science-review/article/transparency-protest-and-autocratic-instability/6EE68292EBE8374553FC6407FCA42A27
- Hellmeier & Bernhard 2023, *CPS* 56(12):1858–1890: https://journals.sagepub.com/doi/10.1177/00104140231152793 ; open PDF https://www.ssoar.info/ssoar/bitstream/handle/document/94893/ssoar-cps-2023-12-hellmeier_et_al-Regime_Transformation_From_Below_Mobilization.pdf
- Knutsen, Dahlum, Rasmussen & Wig, *CPS*: https://journals.sagepub.com/doi/10.1177/00104140251369338 ; V-Dem policy brief 45 https://www.v-dem.net/media/publications/PB_45.pdf
- Gerschewski 2013, *Democratization* 20(1):13–38: https://www.tandfonline.com/doi/abs/10.1080/13510347.2013.738860 ; open PDF https://www.econstor.eu/bitstream/10419/200982/1/f-17712-full-text-Gerschewski-Pillars-v2.pdf
- Gerschewski 2018, *Perspectives on Politics* 16(3):652–665: https://www.cambridge.org/core/journals/perspectives-on-politics/article/abs/legitimacy-in-autocracies-oxymoron-or-essential-feature/ABFD028D0BF3A11BBF553D66D3993C8E ; open repository https://edoc.hu-berlin.de/items/da054693-6764-4d20-860c-72b65c48373b
- Dukalskis & Gerschewski 2017, *Contemporary Politics* 23(3):251–268: https://www.tandfonline.com/doi/abs/10.1080/13569775.2017.1304320
- von Soest & Grauvogel 2017, *Contemporary Politics* 23(3): https://www.tandfonline.com/doi/abs/10.1080/13569775.2017.1304319 ; Grauvogel & von Soest 2014, *EJPR* 53(4):635–653 https://onlinelibrary.wiley.com/doi/abs/10.1111/1475-6765.12065 ; Oxford Handbook chapter https://academic.oup.com/edited-volume/55828/chapter/454404197 ; GIGA page https://www.giga-hamburg.de/en/publications/contributions/legitimacy-and-legitimation-strategies-in-authoritarian-regimes
- Tannenberg et al., RLS: V-Dem WP 86 https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3378017 ; *EPSR* 13(1):77–94 https://www.cambridge.org/core/journals/european-political-science-review/article/claiming-the-right-to-rule-regime-legitimation-strategies-from-1900-to-2019/6394D262B708B6199B115DFFEE11BE7C
- Dickson 2016: https://global.oup.com/academic/product/the-dictators-dilemma-9780190228552

**Part 6 — asymmetry**
- Valgarðsson et al. 2025, *BJPS* 55:e15: https://www.cambridge.org/core/journals/british-journal-of-political-science/article/crisis-of-political-trust-global-trends-in-institutional-trust-from-1958-to-2019/7EF4EDA709F27C691380CFC1BCECF6B8 (DOI 10.1017/S0007123424000498)
- Waldner & Lust 2018, *ARPS* 21:93–113: https://www.annualreviews.org/content/journals/10.1146/annurev-polisci-050517-114628 ; PDF https://jrspringman.github.io/psci3200-globaldev/files/waldner-lust-2018-unwelcome-change-coming-to-terms-with-democratic-backsliding.pdf
- Boese, Edgell, Hellmeier, Maerz & Lindberg 2021, *Democratization* 28(5):885–907: https://www.tandfonline.com/doi/full/10.1080/13510347.2021.1891413
- Jacob 2025, *EJPR* 64(1):348–373: https://onlinelibrary.wiley.com/doi/full/10.1111/1475-6765.12703 (DOI 10.1111/1475-6765.12703)
- Neundorf, Ozturk, Northmore-Ball, Tertytchnaya & Gerschewski 2025, *CPS* 58(9):1854–1889: https://journals.sagepub.com/doi/10.1177/00104140241283006 ; open repository https://eprints.gla.ac.uk/332242
- Matovski 2021, *Popular Dictatorships* (Cambridge UP): https://www.cambridge.org/core/books/popular-dictatorships/D7B24EB0DE0D44E5C154F83D2E8A84C4
- Norris 1999 *Critical Citizens*: https://global.oup.com/academic/product/critical-citizens-9780198295686 ; Dalton 2004: https://academic.oup.com/book/36196
- Magalhães (working PDF, verify): https://www.ics.ulisboa.pt/fcteval/papers/Magalhaes14.pdf
- Katz & Levin, *Social Science Research* (verify): https://www.sciencedirect.com/science/article/abs/pii/S0049089X17301369

**Part 7 — recency**
- Sato & Wiebrecht 2024, *PRQ* 77(3):1010–1025: https://pmc.ncbi.nlm.nih.gov/articles/PMC11305955/ (DOI 10.1177/10659129241252811) ; V-Dem WP 144 https://www.v-dem.net/media/publications/wp_144.pdf ; SSRN https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4569010
- Maerz, Edgell, Wilson, Hellmeier & Lindberg 2024, *JPR* 61(6):967–984 (ERT): https://journals.sagepub.com/doi/10.1177/00223433231168192
- Ma, Choi, Tai, Hu & Solt 2026, *EPSR* FirstView: https://www.cambridge.org/core/journals/european-political-science-review/article/macrodiscontent-across-countries/652A1840F9AB9E5AF5A50529AA05047B (DOI 10.1017/S1755773926100599)
- "Strategies of political control and regime survival in autocracies," *Democratization* 2025: https://www.tandfonline.com/doi/full/10.1080/13510347.2025.2611256
- Baker, Eltokhy & Guo 2026, arXiv:2607.05534: https://arxiv.org/pdf/2607.05534
- V-Dem Democracy Report 2025: https://www.v-dem.net/documents/54/v-dem_dr_2025_lowres_v1.pdf ; Democracy Report 2026: https://v-dem.net/documents/75/V-Dem_Institute_Democracy_Report_2026_lowres.pdf

**Checked and could not verify (do not cite without further work)**
- Kalinin 2016 (upward bias in measured electoral support, Russia) — secondary citation only
- Tang 2016 (list experiment, ~4% misreporting in China) — secondary citation only, probably *Populist Authoritarianism* (Oxford UP)
- Schedler 1999 (skepticism about survey-measured autocratic legitimacy) — standard secondhand citation, outlet unconfirmed
- Chen, Zhong & Hillard 1997; Zhong & Chen 2013 *Asian Survey*; Lü 2014 *APSR* 108(2) — secondary description only
- Li, Shi & Zhu (China list experiment, ~25-pp gap) — secondary description only
- Gerschewski 2018 body text — only abstract/metadata retrieved; PDF at https://doi.org/10.18452/21703 could not be text-extracted
- Claassen 2020 PDF body text — could not be text-extracted; the autocracy null is verified from the 2024 erratum figures and the published abstract/summary, not from the article body
- "Strategies of political control and regime survival in autocracies" — author names not captured

---

## Implications for positioning

### How strong is the autocracy-gap claim?

**Strong on the narrow version; overstated on the broad version.** Three
separate lines of evidence converge, and each is independently checkable:

1. **From the DV side** (prior review, Part 3): none of 18 canonical
   regime-durability models includes mass opinion as a predictor.
2. **From the IV side** (this review, Part 5): no published study puts an
   aggregate mass-attitude measure on the right-hand side of a coded autocratic
   regime-change event. The nearest thing, Claassen 2020, uses a democracy
   *score*, tests democratic support rather than discontent, and returns a null
   in autocracies (erratum: 4.40, 95% CI [−23.00, 25.23]).
3. **From the literature's own testimony** (Part 6): the leading global trust
   series excludes autocracies from its headline analysis by design; the leading
   scholars of autocratic legitimacy state that they study regime claims because
   citizen legitimacy is "notoriously difficult to measure with public opinion
   surveys."

The third line is what makes this more than an absence-of-evidence argument. The
gap is not an oversight; it is a **documented methodological retreat**, and the
paper can name the retreat and the reason for it.

**Where the claim is vulnerable:**

- **"Severely under-developed" invites a counterexample hunt.** Anyone can point
  at Guriev & Treisman (140+ countries), Mauk (100+ countries), Gilley (72
  countries), Matovski, Neundorf et al. and say the autocracy attitude literature
  is not thin at all. It is not thin — it is thin **in one specific cell**. The
  phrasing must carry the DV restriction, or it will read as ignorance of a large
  literature.
- **The design is no longer novel, only the IV is.** Sato & Wiebrecht 2024 (*PRQ*)
  already publishes ERT episode-onset, split by regime type, with a regime-side
  IV. Claiming novelty for the framework rather than the variable would be an
  easy target.
- **The measurement objection is live and partly unanswered.** Tannenberg 2022 in
  particular documents a bias that is *conditional on regime type* — precisely the
  dimension the paper's interaction exploits. The paper currently says nothing
  about this anywhere.

### Recommended phrasing

Preferred, for the literature review or introduction:

> Scholarship on political support has developed almost entirely within
> democracies. Its canonical statements are framed around advanced industrial
> democracies, and even the most comprehensive global compilation of institutional
> trust restricts its headline analyses to countries coded as democracies for a
> majority of years [Valgarðsson et al. 2025]. Where autocracies have been studied,
> researchers have turned away from citizen attitudes altogether, measuring instead
> what regimes *claim* about their right to rule, on the explicit grounds that
> autocratic legitimacy is difficult to recover from public opinion surveys
> [Tannenberg et al. 2021; Grauvogel & von Soest 2024]. The result is that no
> cross-national study has yet asked whether what autocratic publics actually think
> predicts whether their regimes come apart. The one attempt closest to this
> question tested support for democracy against changes in the level of democracy
> and found no association in autocracies [Claassen 2020].

Compact version, for an abstract or contribution paragraph:

> Existing cross-national tests of mass attitudes and regime change are almost
> exclusively about democracies, and the few that include autocracies use
> continuous democracy scores rather than coded regime events. Whether public
> discontent predicts the onset of regime change in autocracies has not been tested.

**Avoid:** "no one has studied mass attitudes in autocracies" (false);
"the literature ignores autocracies" (false); "severely under-developed" as a
bare adjective without the DV restriction attached.

### Three concrete recommendations

1. **Add a measurement-robustness paragraph** to the Data section naming
   Tannenberg 2022, Robinson & Tannenberg 2019 and Carter et al. 2024, and
   making the three-part reply: every documented bias is pro-regime and therefore
   attenuates the discontent coefficient; the bias literature concerns levels
   while the estimand is variation; Shen & Truex 2021 find near-null self-censorship
   in most autocracies, with China the outlier. Note the counter-current
   (Shamaileh 2025 attacks Shen & Truex's instrument; Frye et al. 2023 show the
   list-experiment corrective is itself unreliable), which supports the position
   that no accepted instrument exists — an argument for proceeding, not for silence.
2. **Cite Sato & Wiebrecht 2024** where the ERT-onset design is introduced, and
   state the difference in one clause (regime-side information manipulation vs.
   citizen-side attitudes).
3. **Fix the bib entry** for the measurement companion: Ma, Choi, Tai, Hu & Solt
   2026, *European Political Science Review*, FirstView (12 Aug 2026), DOI
   10.1017/S1755773926100599. It is currently cited in the qmd without an outlet.
