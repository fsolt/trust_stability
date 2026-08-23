/*
 * Zotero 7 "Run JavaScript" script
 * Run via: Tools > Developer > Run JavaScript (Zotero desktop)
 *
 * Purpose
 * -------
 * Replaces the incomplete stub items (created by an earlier sync script,
 * exported to paper/missed.bib, tagged "needs-review") with verified,
 * complete bibliographic information researched from publisher pages /
 * DOI records for each work. It does NOT create new items or new
 * collection filings — it only overwrites fields on the existing stub
 * items already sitting in "p_dcpo_trustRegime_main".
 *
 * Matching: each stub item is located by (a) the "needs-review" tag AND
 * (b) first-author last name + publication year, which is enough to
 * disambiguate all 14 works below (no collisions in this set).
 *
 * Two entries (Alexander & Welzel 2017; Voeten 2017) are Journal of
 * Democracy "Web Exchange" pieces, not paginated journal-issue articles:
 * they legitimately have no volume/issue/page numbers. Voeten 2017's DOI
 * is an SSRN preprint DOI (10.2139/ssrn.2882878), not a journal DOI —
 * that is the only DOI that exists for that piece.
 *
 * On success: the "needs-review" tag is removed and replaced with
 * "review-completed", and the Extra field's stray "Citation Key: ..."
 * placeholder note is cleared (Better BibTeX will regenerate a proper key
 * from the now-complete metadata).
 *
 * Safety
 * ------
 * DRY_RUN defaults to true: it only logs what it WOULD change. Read the
 * log, then set DRY_RUN = false and re-run to actually apply the updates.
 *
 * API calls used here (verified against Zotero source / official docs,
 * consistent with the sibling scripts in this folder):
 *   new Zotero.Search(); s.libraryID; s.addCondition('tag','is','needs-review'); s.search()
 *   Zotero.Items.getAsync(ids)
 *   item.getField / item.getCreators / item.setField / item.setCreators
 *   item.removeTag / item.addTag / item.saveTx()
 */

const DRY_RUN = true;

// ---------------------------------------------------------------------
// Verified replacement data (researched from publisher pages / DOI
// records; see chat log for per-item source notes and confidence flags).
// ---------------------------------------------------------------------
const UPDATES = [
  {
    matchLastName: 'Alexander', matchYear: 2017,
    title: 'The Myth of Deconsolidation: Rising Liberalism and the Populist Reaction',
    creators: [
      { firstName: 'Amy C.', lastName: 'Alexander', creatorType: 'author' },
      { firstName: 'Christian', lastName: 'Welzel', creatorType: 'author' },
    ],
    publicationTitle: 'Journal of Democracy (Web Exchange)',
    volume: '', issue: '', pages: '', doi: '',
    url: 'https://www.journalofdemocracy.org/wp-content/uploads/2018/12/Journal-of-Democracy-Web-Exchange-Alexander-and-Welzel.pdf',
    extraNote: 'Online-only web exchange piece; no volume/issue/pages/DOI assigned by the publisher.',
  },
  {
    matchLastName: 'Voeten', matchYear: 2017,
    title: 'Are People Really Turning Away from Democracy?',
    creators: [{ firstName: 'Erik', lastName: 'Voeten', creatorType: 'author' }],
    publicationTitle: 'Journal of Democracy (Web Exchange)',
    volume: '', issue: '', pages: '', doi: '10.2139/ssrn.2882878',
    url: 'https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2882878',
    extraNote: 'Online-only web exchange piece (also circulated as an SSRN working paper); DOI is the SSRN preprint DOI, no journal DOI exists.',
  },
  {
    matchLastName: 'Belkin', matchYear: 2003,
    title: 'Toward a Structural Understanding of Coup Risk',
    creators: [
      { firstName: 'Aaron', lastName: 'Belkin', creatorType: 'author' },
      { firstName: 'Evan', lastName: 'Schofer', creatorType: 'author' },
    ],
    publicationTitle: 'Journal of Conflict Resolution',
    volume: '47', issue: '5', pages: '594-620', doi: '10.1177/0022002703258197',
    url: 'https://journals.sagepub.com/doi/10.1177/0022002703258197',
    extraNote: '',
  },
  {
    matchLastName: 'Bernhard', matchYear: 2001,
    title: 'Economic Performance, Institutional Intermediation, and Democratic Survival',
    creators: [
      { firstName: 'Michael', lastName: 'Bernhard', creatorType: 'author' },
      { firstName: 'Timothy', lastName: 'Nordstrom', creatorType: 'author' },
      { firstName: 'Christopher', lastName: 'Reenock', creatorType: 'author' },
    ],
    publicationTitle: 'Journal of Politics',
    volume: '63', issue: '3', pages: '775-803', doi: '10.1111/0022-3816.00087',
    url: 'https://www.journals.uchicago.edu/doi/10.1111/0022-3816.00087',
    extraNote: '',
  },
  {
    matchLastName: 'Celestino', matchYear: 2013,
    title: 'Fresh Carnations or All Thorn, No Rose? Nonviolent Campaigns and Transitions in Autocracies',
    creators: [
      { firstName: 'Mauricio Rivera', lastName: 'Celestino', creatorType: 'author' },
      { firstName: 'Kristian Skrede', lastName: 'Gleditsch', creatorType: 'author' },
    ],
    publicationTitle: 'Journal of Peace Research',
    volume: '50', issue: '3', pages: '385-400', doi: '10.1177/0022343312469979',
    url: 'https://journals.sagepub.com/doi/10.1177/0022343312469979',
    extraNote: '',
  },
  {
    matchLastName: 'Gasiorowski', matchYear: 1995,
    title: 'Economic Crisis and Political Regime Change: An Event History Analysis',
    creators: [{ firstName: 'Mark J.', lastName: 'Gasiorowski', creatorType: 'author' }],
    publicationTitle: 'American Political Science Review',
    volume: '89', issue: '4', pages: '882-897', doi: '10.2307/2082515',
    url: 'https://www.cambridge.org/core/journals/american-political-science-review/article/abs/economic-crisis-and-political-regime-change-an-event-history-analysis/11C9EEC4005FFE27B06A49AB60489722',
    extraNote: '',
  },
  {
    matchLastName: 'Lührmann', matchYear: 2019,
    title: 'A Third Wave of Autocratization Is Here: What Is New About It?',
    creators: [
      { firstName: 'Anna', lastName: 'Lührmann', creatorType: 'author' },
      { firstName: 'Staffan I.', lastName: 'Lindberg', creatorType: 'author' },
    ],
    publicationTitle: 'Democratization',
    volume: '26', issue: '7', pages: '1095-1113', doi: '10.1080/13510347.2019.1582029',
    url: 'https://www.tandfonline.com/doi/full/10.1080/13510347.2019.1582029',
    extraNote: 'Pages/DOI corroborated by multiple secondary sources; publisher page blocked direct verification.',
  },
  {
    matchLastName: 'MacCulloch', matchYear: 2004,
    title: 'The Impact of Income on the Taste for Revolt',
    creators: [{ firstName: 'Robert', lastName: 'MacCulloch', creatorType: 'author' }],
    publicationTitle: 'American Journal of Political Science',
    volume: '48', issue: '4', pages: '830-848', doi: '10.1111/j.0092-5853.2004.00104.x',
    url: 'https://doi.org/10.1111/j.0092-5853.2004.00104.x',
    extraNote: '',
  },
  {
    matchLastName: 'Maerz', matchYear: 2023,
    title: 'Episodes of Regime Transformation',
    creators: [
      { firstName: 'Seraphine F.', lastName: 'Maerz', creatorType: 'author' },
      { firstName: 'Amanda B.', lastName: 'Edgell', creatorType: 'author' },
      { firstName: 'Matthew C.', lastName: 'Wilson', creatorType: 'author' },
      { firstName: 'Sebastian', lastName: 'Hellmeier', creatorType: 'author' },
      { firstName: 'Staffan I.', lastName: 'Lindberg', creatorType: 'author' },
    ],
    publicationTitle: 'Journal of Peace Research',
    volume: '61', issue: '6', pages: '967-984', doi: '10.1177/00223433231168192',
    url: 'https://journals.sagepub.com/doi/10.1177/00223433231168192',
    extraNote: 'Published online 2023, print issue 2024; page range not independently re-verified on the publisher page.',
  },
  {
    matchLastName: 'Muller', matchYear: 1985,
    title: 'Income Inequality, Regime Repressiveness, and Political Violence',
    creators: [{ firstName: 'Edward N.', lastName: 'Muller', creatorType: 'author' }],
    publicationTitle: 'American Sociological Review',
    volume: '50', issue: '1', pages: '47-61', doi: '10.2307/2095339',
    url: 'https://doi.org/10.2307/2095339',
    extraNote: 'The URL previously attached to this stub (cambridge.org/.../inequality-and-political-violence-revisited/...) was actually Wang, Dixon, Muller & Seligson (1993) APSR 87(4), a different, later paper — not used here.',
  },
  {
    matchLastName: 'Saikkonen', matchYear: 2023,
    title: 'Guardians of Democracy or Passive Bystanders? A Conjoint Experiment on Elite Transgressions of Democratic Norms',
    creators: [
      { firstName: 'Inga A.-L.', lastName: 'Saikkonen', creatorType: 'author' },
      { firstName: 'Henrik Serup', lastName: 'Christensen', creatorType: 'author' },
    ],
    publicationTitle: 'Political Research Quarterly',
    volume: '76', issue: '1', pages: '127-142', doi: '10.1177/10659129211073592',
    url: 'https://journals.sagepub.com/doi/10.1177/10659129211073592',
    extraNote: 'Correction: journal is Political Research Quarterly, not Comparative Political Studies as originally logged in the lit review.',
  },
  {
    matchLastName: 'Svolik', matchYear: 2008,
    title: 'Authoritarian Reversals and Democratic Consolidation',
    creators: [{ firstName: 'Milan W.', lastName: 'Svolik', creatorType: 'author' }],
    publicationTitle: 'American Political Science Review',
    volume: '102', issue: '2', pages: '153-168', doi: '10.1017/S0003055408080143',
    url: 'https://www.cambridge.org/core/journals/american-political-science-review/article/abs/authoritarian-reversals-and-democratic-consolidation/E3963766F74CC6CF2AEE2F5B883139F4',
    extraNote: '',
  },
  {
    matchLastName: 'Ulfelder', matchYear: 2007,
    title: 'Natural-Resource Wealth and the Survival of Autocracy',
    creators: [{ firstName: 'Jay', lastName: 'Ulfelder', creatorType: 'author' }],
    publicationTitle: 'Comparative Political Studies',
    volume: '40', issue: '8', pages: '995-1018', doi: '10.1177/0010414006287238',
    url: 'https://journals.sagepub.com/doi/10.1177/0010414006287238',
    extraNote: '',
  },
  {
    matchLastName: 'Welzel', matchYear: 2007,
    title: 'Are Levels of Democracy Affected by Mass Attitudes? Testing Attainment and Sustainment Effects on Democracy',
    creators: [{ firstName: 'Christian', lastName: 'Welzel', creatorType: 'author' }],
    publicationTitle: 'International Political Science Review',
    volume: '28', issue: '4', pages: '397-424', doi: '10.1177/0192512107079640',
    url: 'https://journals.sagepub.com/doi/10.1177/0192512107079640',
    extraNote: '',
  },
];

const log = [];
const say = (s) => { log.push(s); Zotero.debug('[trustRegime-fillmissed] ' + s); };

// ---------------------------------------------------------------------
// 1. Find every item tagged "needs-review" in My Library
// ---------------------------------------------------------------------
const libraryID = Zotero.Libraries.userLibraryID;
const s = new Zotero.Search();
s.libraryID = libraryID;
s.addCondition('tag', 'is', 'needs-review');
const ids = await s.search();
const stubItems = await Zotero.Items.getAsync(ids);
say(`Found ${stubItems.length} item(s) tagged "needs-review" in My Library.`);

function extractYear(dateStr) {
  const m = (dateStr || '').match(/\d{4}/);
  return m ? parseInt(m[0], 10) : null;
}
function normName(s) {
  return (s || '').normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
}

function findStub(update) {
  return stubItems.find((item) => {
    let creators = [];
    try { creators = item.getCreators(); } catch (e) { creators = []; }
    const hasLastName = creators.some((c) => normName(c.lastName) === normName(update.matchLastName));
    return hasLastName && extractYear(item.getField('date')) === update.matchYear;
  });
}

// ---------------------------------------------------------------------
// 2. Match each verified update to its stub item
// ---------------------------------------------------------------------
const matched = [];
const unmatched = [];
for (const u of UPDATES) {
  const item = findStub(u);
  if (item) matched.push({ u, item });
  else unmatched.push(u);
}

say(`\n${matched.length} of ${UPDATES.length} verified citation(s) matched to a stub item.`);
for (const { u, item } of matched) {
  say(`  - ${u.matchLastName} ${u.matchYear} -> item id ${item.id} ("${item.getField('title')}")`);
}
if (unmatched.length) {
  say(`\nWARNING: ${unmatched.length} verified citation(s) found NO matching stub item (already fixed, or tag/author/year mismatch):`);
  for (const u of unmatched) say(`  - ${u.matchLastName} ${u.matchYear}: ${u.title}`);
}

if (DRY_RUN) {
  say('\nDRY_RUN is true — no changes made. Review the list above, set DRY_RUN = false, and re-run to apply.');
  return log.join('\n');
}

// ---------------------------------------------------------------------
// 3. Apply: overwrite fields on the matched stub items
// ---------------------------------------------------------------------
let updated = 0;
for (const { u, item } of matched) {
  item.setField('title', u.title);
  item.setCreators(u.creators);
  const trySet = (field, value) => { if (value) { try { item.setField(field, value); } catch (e) { /* n/a for this item type */ } } };
  trySet('publicationTitle', u.publicationTitle);
  trySet('volume', u.volume);
  trySet('issue', u.issue);
  trySet('pages', u.pages);
  trySet('DOI', u.doi);
  trySet('url', u.url);
  item.setField('extra', u.extraNote || '');
  item.removeTag('needs-review');
  item.addTag('review-completed');
  await item.saveTx();
  updated++;
  say(`Updated item ${item.id}: "${u.title}"`);
}

say(`\nDone: updated ${updated} item(s) with verified bibliographic data.`);
return log.join('\n');
