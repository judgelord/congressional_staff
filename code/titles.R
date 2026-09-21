
library(tidyverse)
library(tidyr)

titles_raw <- here::here("data", "legistorm", "Legistorm Data Purchase", "salary_title.csv") |> read_csv()

titles_raw <- here::here("data", "legistorm", "LegistormDataWisconsin2026", "Data", "salary_auto_history.csv") |> read_csv() |>
  rename(full_name = title)

head(titles_raw)

# A robust approach is to:
#
#   1. preserve the original title;
# 2. split only on a slash surrounded by optional spaces;
# 3. minimally normalize each half;
# 4. classify the first title;
# 5. use the second title only when the first is uninformative.
#
# Your pasted titles appear truncated, so the rules below are a framework you can refine against the complete values.
#
# ```r



# Replace `job_title` with your actual title-column name.
titles_clean <- titles_raw %>%
  mutate(
    title_original = full_name,
    title_original = str_squish(title_original)
  ) %>%
  separate_wider_delim(
    title_original,
    delim = "/",
    names = c("title_1", "title_2"),
    too_few = "align_start",
    too_many = "merge"
  ) %>%
  mutate(
    across(
      c(title_1, title_2),
      ~ .x %>%
        str_to_lower() %>%
        str_replace_all("&", " and ") %>%
        str_replace_all("[[:punct:]]+", " ") %>%
        str_squish() %>%
        na_if("")
    )
  )
# ```
#
# This standardization intentionally retains meaningful words while making capitalization and punctuation irrelevant.
#
# ## Classification function
#
# The function returns a detailed subcategory. Rules are ordered from relatively specific to general, because `case_when()` uses the first matching condition.
#

# A few consequential classifications in this revision are:
#
#   professional staff member coast guard and maritime transportation → policy: military and veterans affairs, because topical rules precede the generic professional-staff rule.
# professional staff energy subcommittee → policy: environment.
# professional staff health subcommittee → policy: health.
# shared staffer professional staff → policy: professional committee staff because the professional-staff rule currently precedes shared staff. Move the shared-staff rule earlier if shared status should take priority.
# Bare terms such as director, staff, office, and program → unclear: generic title, rather than forcing them into a substantive category.
# Titles such as acting director education and training classify from their informative topic, because exact generic-title matching only catches titles consisting entirely of generic terms.

# Topic-specific rules precede broad adviser rules, so energy adviser, senior tax adviser, and health adviser become policy roles.
# Constituent rules precede communications, so constituent correspondence coordinator remains constituent casework rather than general correspondence.
# Geographic and community rules precede generic director, representative, and liaison rules.
# Temporary rules occur near the top, so assistant food manager temporary and associate staff part time are temporary regardless of their substantive title.
# Generic labels such as assistant, director, manager, liaison, and representative are marked unclear rather than assigned based only on rank.
# Date-appended senior adviser records are classified as generic advisers. If those strings represent payments rather than job titles, move them into "other expenses" instead.
#
# # ```r
# classify_title <- function(x) {
#   case_when(
#     is.na(x) | x == "NA" ~ NA_character_,
#
#     # Intergovernmental, oversight, and investigations
#     str_detect(
# x,
# "\\b(intergovernmental|oversight|investigations?|inspectors?|federal liaison|government and community relations)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "intergovernmental",
#
#     # Legislative leadership
#     str_detect(x, "\\blegislative director\\b") ~
# "legislative: legislative leadership",
#
#     # Legislative staff
#     str_detect(
# x,
# "\\b(senior legislative assistant|legislative assistant|legislative correspondent|legislative counsel)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "legislative: legislative staff",
#
#     # Broad legislative rule
#     str_detect(x, "\\blegislative\\b") ~
# "legislative: other legislative",
#
#     # Specific policy areas: appropriations
#     str_detect(x, "\\bappropriations?\\b") ~
# "policy: appropriations",
#
#     # Military, veterans, maritime transportation, and Coast Guard
#     str_detect(
# x,
# "\\b(veterans affairs|military|coast guard|maritime transportation)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: military and veterans affairs",
#
#     # Agriculture
#     str_detect(x, "\\b(agriculture|agricultural)\\b" |> str_remove_all("\n") |> str_squish() ) ~
# "policy: agriculture",
#
#     # Environment and energy
#     str_detect(
# x,
# "\\b(conservation|sustainability|environmental|environment|natural resources|energy subcommittee)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: environment",
#
#     # Health
#     str_detect(
# x,
# "\\b(health subcommittee|health and welfare|health policy)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: health",
#
#     # Education policy
#     str_detect(
# x,
# "\\b(education subcommittee|education policy)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: education",
#
#     # Technology, innovation, science, and space policy
#     str_detect(
# x,
# "\\b(technology and innovation|technology subcommittee|space subcommittee|staff scientist)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: science and technology",
#
#     # Foreign policy
#     str_detect(
# x,
# "\\b(war|defense|foreign affairs|asian|international)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: foreign",
#
#     # Economic development and economics
#     str_detect(
# x,
# "\\b(economic development|economist|economic policy)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: economic",
#
#     # Professional committee staff
#     str_detect(
# x,
# "\\b(professional staff|professional staff member|senior professional staff|staff associate)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: professional committee staff",
#
#     # Investigative staff
#     str_detect(
# x,
# "\\b(investigator|auditor)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: oversight and investigations",
#
#     # Policy leadership
#     str_detect(x, "\\bpolicy director\\b") ~
# "policy: policy director",
#
#     # General policy
#     str_detect(
# x,
# "\\b(policy advisor|policy adviser|policy|librarian|committee|counsel|research|researcher|analyst|attorney)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: policy general",
#
#     # Temporary positions
#     str_detect(
# x,
# "\\b(intern|internship|fellow|fellowship|page)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "temporary: intern or fellow",
#
#     # Constituent casework
#     str_detect(
# x,
# "\\b(caseworker|case work|casework|case manager|constituent services?|constituent advocate|constituency|constituent liaison|veterans? services?|immigration services?|immigration specialist)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "constituent casework: casework",
#
#     # Correspondence
#     str_detect(
# x,
# "\\b(director of correspondence|correspondence director|correspondence manager|correspondence specialist|correspondent specialist|deputy director correspondence)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "communications: correspondence",
#
#     # Speechwriting
#     str_detect(
# x,
# "\\b(speechwriter|speech writer|speechwriting)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "communications: speechwriting",
#
#     # Press and communications
#     str_detect(
# x,
# "\\b(communications? director|communications? deputy|communications? advisor|broadcast|communications?|communications? specialist|communications? assistant|press secretary|external affairs|media|press|transition|deputy press secretary|spokesperson|spokesman)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "communications: press and communications",
#
#     # Digital and creative
#     str_detect(
# x,
# "\\b(digital|social media|webmaster|reprographics|imaging|photography|videographer|video editor|photographer|graphic design|graphics?|art director|web|producer|radio technician|editor|public affairs)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "communications: digital and creative",
#
#     # Leadership: senior office leadership
#     str_detect(
# x,
# "\\b(chief of staff|staff director|executive director)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "leadership team: senior office leadership",
#
#     # Leadership: policy
#     str_detect(
# x,
# "\\b(chief deputy|chief counsel|general counsel)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "leadership team: policy leadership",
#
#     # Leadership: district
#     str_detect(
# x,
# "\\b(district director|state director|regional director|manhattan director)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "leadership team: district leadership",
#
#     # Advance and protocol
#     str_detect(
# x,
# "\\b(advance assistant|advance associate|advance coordinator|advance deputy|advance director|advance representative|director of protocol|protocol director)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "leadership team: advance and protocol",
#
#     # District outreach and field
#     str_detect(
# x,
# "\\b(district|field|area|region|regional|community liaison|outreach|central|eastern|western|southern|northern|business liaison|community development|manhattan)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "district team: outreach and field",
#
#     # Education and training
#     str_detect(
# x,
# "\\b(teacher|instructor|academic|academy|instructional|education and training|strategic learning and development|training and development)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "instruction",
#
#     # Information technology and systems
#     str_detect(
# x,
# "\\b(computer specialist|systems?|telecommunications services|systems development services|network|information technology|it director|database|business continuity)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: information technology",
#
#     # Programs and projects
#     str_detect(
# x,
# "\\b(program manager|senior program manager|project assistant|project director|project manager|projects assistant|projects director|special projects coordinator|director of special projects)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: programs and projects",
#
#     # Mailroom and inventory
#     str_detect(
# x,
# "\\b(mailroom assistant|inventory specialist|manager textiles|laborer)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: facilities and logistics",
#
#     # Administrative office operations
#     str_detect(
# x,
# "\\b(chief administrative officer|administrative director|administrator|administrator assistant|administration|information|scheduling|accounts|payable|acquisitions?|appointments?|administrative assistant|office manager|office administrator|office director|account|accountant|accounting|enterprise|retail|logistics|equipment|technical|grants|professional assistant|operations? director|scheduler|scheduling director|executive assistant|trip coordinator|vendor|visitor|compensation|stationery|staff assistant|receptionist|systems administrator|captioning|workflow|office coordinator|personal assistant|secretary|printing|pay|staff professional|contract|audit|contracting|records?|payroll|finance|financial|budget|human resources?|d c office|member services|group assistant|support|contractor|engineering|engineer|desk|administrative|operations)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: office operations",
#
#     # Generic special assistants and congressional assistants
#     str_detect(
# x,
# "\\b(special assistant|congressional assistant|advance assistant)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: executive and staff support",
#
#     # Support services
#     str_detect(
# x,
# "\\b(driver|chauffeur|security|facilities|mail|messenger|aide|police|doorkeeper|operator|foreman|journeyman|cloakroom|special events|assistant to|assist to|child care|credentialing|counselor|services|technician|supervisor)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: support services",
#
#     # Special institutional roles
#     str_detect(
# x,
# "\\b(chaplain|clerk|floor|sergeant|private first class|apprentice|archival|archives|archivist|conservator|bookbinder|curator|guide|tours|parliamentarian|historian)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "administrative: other",
#
#     # Shared staff
#     str_detect(
# x,
# "\\b(shared staff|shared staffer|shared employee)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "unclear: shared staff",
#
#     # Generic staff labels
#     str_detect(
# x,
# "\\b(senior staff associate|staff associate|professional staff|professional staff member|senior professional staff)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "policy: professional committee staff",
#
#     # Party roles
#     str_detect(x, "\\b(democratic|republican|majority|minority)\\b" |> str_remove_all("\n") |> str_squish() ) ~
# "party: other",
#
#     # Other expenses
#     str_detect(
# x,
# "\\b(canceled check|stop payment|overpayment|allowance|consultant)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "other expenses",
#
#     # Unclear generic titles
#     str_detect(
# x,
# "^(office|program|staff|director|assistant director|deputy director|acting director|acting associate director|acting manager)$"
#     ) ~ "unclear: generic title",
#
#     str_detect(
# x,
# "\\b(shared staff full committee|shared employee)\\b" |> str_remove_all("\n") |> str_squish()
#     ) ~ "unclear",
#
#     TRUE ~ NA_character_
#   )
# }


# V2

# Below is a revised version that expands coverage while preserving cascading priority.
#  normalize `x` inside the function, so literal `"NA"`, `"not listed"`, and `"no title listed"` become missing.

classify_title <- function(x) {

  x <- x |>
    stringr::str_to_lower() |>
    stringr::str_squish()

  x[x %in% c("", "na", "n/a", "not listed", "no title listed")] <- NA_character_

  dplyr::case_when(
    is.na(x) ~ NA_character_,

    # ------------------------------------------------------------------
    # Non-title payments and leave records
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(canceled check|stop payment|overpayment|allowance|consultant|\
overtime payment|payment from prior reporting period|lump sum annual leave|\
unpaid leave|lwop employee|expense transfer)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "other expenses",

    # Other expenses
    str_detect(
      x,
      "\\b(canceled check|stop payment|overpayment|allowance|consultant|unpaid leave)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "other expenses",

    # ------------------------------------------------------------------
    # Temporary positions
    # ------------------------------------------------------------------
    # Temporary positions
    str_detect(
      x,
      "\\b(intern|internship|fellow|fellowship|page|graduate|temporary|temp)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "temporary: intern or fellow",

    str_detect(
      x,
      "\\b(intern|internship|fellow|fellowship|page|extern|\
temporary|part time|summer associate|spring associate|fall associate|\
graduate associate|graduate student|student assistant|special government employee)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "temporary: intern or fellow",

    # ------------------------------------------------------------------
    # Intergovernmental, government relations, oversight, investigations
    # ------------------------------------------------------------------
    # Intergovernmental, oversight, and investigations
    str_detect(
      x,
      "\\b(intergovernmental|oversight|investigations?|investigative|inspectors?|
federal liaison|government and community relations|regulartory|regulatory|government relations)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "intergovernmental",

    str_detect(
      x,
      "\\b(intergovernmental|inter governmental|intragovernmental|\
governmental affairs|government affairs|governmental relations|\
government relations|congressional relations|congressional liaison|\
federal liaison|federal funding liasion|gpo liaison|\
congressional delegation representative|oversight|investigations?|\
judiciary|\
investigative|investigator|inspectors?|auditor|detective|special agent|\
specialist agent|law enforcement liaison|government performance task force)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "intergovernmental",

    # ------------------------------------------------------------------
    # Legislative
    # ------------------------------------------------------------------
    # Specific policy areas: appropriations
    str_detect(x, "\\bappropriations?\\b") ~
      "policy: appropriations",

    # Military, veterans, maritime transportation, and Coast Guard
    str_detect(
      x,
      "\\b(veterans affairs|military|coast guard|maritime transportation)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: military and veterans affairs",

    # Agriculture
    str_detect(x, "\\b(agriculture|agricultural)\\b" |> str_remove_all("\n") |> str_squish() ) ~
      "policy: agriculture",

    # Environment and energy
    str_detect(
      x,
      "\\b(conservation|sustainability|environmental|environment|natural resources|natural resource|energy subcommittee|great lakes)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: environment",

    # Health
    str_detect(
      x,
      "\\b(health subcommittee|health and welfare|health policy)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: health",

    # Education policy
    str_detect(
      x,
      "\\b(education subcommittee|education policy)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: education",

    # Technology, innovation, science, and space policy
    str_detect(
      x,
      "\\b(technology and innovation|technology subcommittee|space subcommittee|staff scientist)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: science and technology",

    # Foreign policy
    str_detect(
      x,
      "\\b(war|defense|foreign affairs|asian|international)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: foreign",


    # Economic development and economics
    str_detect(
      x,
      "\\b(immigration)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: immigration",

    # Economic development and economics
    str_detect(
      x,
      "\\b(economic development|economist|economic policy)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: economic",


    # Investigative staff
    str_detect(
      x,
      "\\b(investigator|auditor)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: oversight and investigations",

    # general staff
    str_detect(
      x,
      "\\b(advisor to|issues)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: other advisors",

    #### MORE LEGISLTITIVE
    str_detect(
      x,
      "\\b(legislative director|director of legislation|votes director|\
whip director|caucus planning director|subcommittee director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "legislative: legislative leadership",

    str_detect(
      x,
      "\\b(senior legislative assistant|senior legislation assistant|\
legislative assistant|legislative correspondent|legislative counsel|\
chamber legislation specialist|medical legislation|parliamentary assistant|\
quorum specialist|rules associate|subcommittee assistant|\
subcommittee staff member|subcommittee statistician)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "legislative: legislative staff",

    str_detect(
      x,
      "\\b(legislative|legislation|chamber legislation|votes|recovery act|\
whip coordinator|whip liaison|deputy whip staffer)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "legislative: other legislative",

    # Legislative leadership
    str_detect(x, "\\blegislative director\\b") ~
      "legislative: legislative leadership",

    # Legislative staff
    str_detect(
      x,
      "\\b(senior legislative assistant|legislative assistant|legislative correspondent|legislative counsel)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "legislative: legislative staff",

    # Broad legislative rule
    str_detect(x, "\\blegislative\\b") ~
      "legislative: other legislative",

    # ------------------------------------------------------------------
    # Specific policy areas
    # ------------------------------------------------------------------
    str_detect(x, "\\bappropriations?\\b") ~
      "policy: appropriations",

    str_detect(
      x,
      "\\b(veterans?|military|armed services|coast guard|\
maritime transportation|defense)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: military and veterans affairs",

    str_detect(
      x,
      "\\b(agriculture|agricultural|dairy|farm|farming)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: agriculture",

    str_detect(
      x,
      "\\b(conservation|sustainability|environmental|environment|\
natural resources?|public lands?|forest resources?|energy|\
green the capitol|interior specialist|sportsman)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: environment",

    str_detect(
      x,
      "\\b(health|healthcare|health care|medical|disability|\
wounded warrior|occupational health)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: health",

    str_detect(
      x,
      "\\b(education|higher education|k 12|children s issues|\
workforce development)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: education",

    str_detect(
      x,
      "\\b(science|space|aeronautics|technology assessment|\
technology and innovation)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: science and technology",

    str_detect(
      x,
      "\\b(foreign affairs|foreign relations|african affairs|\
international|asian|europe|eurasia|counterterrorism|intelligence|\
threat assessment|war)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: foreign",

    str_detect(
      x,
      "\\b(economic|economics|economist|macroeconomist|tax|\
banking|business affairs|commerce and industry|trade adviser|\
business development|economic recovery|revenues and economics|\
small business|foreclosure mitigation|labor and economic)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: economic",

    str_detect(
      x,
      "\\b(transportation|transit|housing|rural development)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: transportation and housing",

    str_detect(
      x,
      "\\b(native american|tribal affairs|insular affairs)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: native and tribal affairs",


    str_detect(
      x,
      "\\b(immigration reform)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: immigration",

    # Policy leadership
    str_detect(x, "\\b(policy director|political director)\\b" |> str_remove_all("\n") |> str_squish() ) ~
      "policy: policy director",

    # General policy
    str_detect(
      x,
      "\\b(policy advisor|policy adviser|policy|librarian|committee|subcommittee|counsel|research|researcher|analyst|attorney|library)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: policy general",



    # Professional committee and policy staff
    str_detect(
      x,
      "\\b(policy director|policy advisor|policy adviser|policy analyst|task force|\
issues coordinator|issues director|issues manager|\
senior strategist|adviser on|adviser for|tax adviser|\
economic adviser|education adviser|science adviser|energy adviser|\
health adviser|healthcare adviser|trade adviser|transportation adviser|\
immigration adviser|legal adviser|tribal affairs adviser|\
counterterrorism adviser|benefits adviser|housing adviser|\
public health adviser|chief scientist|chief macroeconomist|\
research|researcher|economist|analyst|attorney|counsel|librarian)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: policy general",

    # POSSIBLE ADDITIONS
    # professional staff|professional member|professional banking staff|\
    # associate staff aviation|special bipartisan staff member|\

    # ------------------------------------------------------------------
    # Constituent casework
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(caseworker|case work|casework|case management|case manager|constituent services?|federal aid assistant|\
federal aid|constituent advocate|constituency|constituent liaison|veterans? services?|immigration services?|immigration specialist|case management)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "constituent casework: casework",

    # Constituent casework
    str_detect(
      x,
      "\\b(caseworker|case worker|case work|casework|case manager|constituent|\
case management|case assistant|case specialist|case officer|\
constituent services?|constituent advocacy|constituent advocate|\
constituent affairs|constituent relations|constituent representative|\
constituent coordinator|constituent liaison|constituent specialist|\
constituent worker|constituency|immigration services?|\
immigration specialist|immigration director|immigrant affairs|\
immigration affairs|immigration issues|\
social worker|social work coordinator|ombudsman|community ombudsman|\
health care advocate|veterans advocate|
federal aid assistant|federal programs assistant)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "constituent casework: casework",

    # Constituent correspondence is retained as casework when explicit
    str_detect(
      x,
      "\\b(constituent correspondent|constituent correspondence)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "constituent casework: correspondence",

    #TODO RECONCILE ABOVE AND BELOW CORRESPONDENCE
    # ------------------------------------------------------------------
    # Communications
    # ------------------------------------------------------------------
    # Correspondence
    str_detect(
      x,
      "\\b(director of correspondence|correspondence director|correspondence manager|correspondence specialist|correspondent specialist|deputy director correspondence)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: correspondence",

    # Speechwriting
    str_detect(
      x,
      "\\b(speechwriter|speech writer|speechwriting)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: speechwriting",

    # Press and communications
    str_detect(
      x,
      "\\b(communications? director|communications? deputy|communications? advisor|broadcast|news|marketing|\
publications|publication|\
communications?|communications? specialist|communications? assistant|press secretary|external affairs|media|press|transition|deputy press secretary|spokesperson|spokesman|public liaison|public relations)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: press and communications",

    # Digital and creative
    str_detect(
      x,
      "\\b(digital|social media|webmaster|reprographics|imaging|creative|design|\
production|\
photography|videographer|video editor|photographer|graphic design|graphics?|art director|web|producer|radio technician|editor|public affairs)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: digital and creative",

    str_detect(
      x,
      "\\b(correspondence|correspondent|personal correspondent)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: correspondence",

    str_detect(
      x,
      "\\b(speechwriter|speech writer|speechwriting|chief writer|\
senior writer|staff writer|writer|historical writer)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: speechwriting and writing",

    str_detect(
      x,
      "\\b(communications?|press|media|spokesperson|spokesman|spokeswoman|\
public relations|public affairs|external affairs|external relations|\
publications?|editorial|rapid response|rapid offense|message planning|\
message event planning|newspaper clipper|creative adviser|\
creative director|community and content coordinator)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: press and communications",

    str_detect(
      x,
      "\\b(digital|social media|webmaster|website|internet adviser|\
interactive design|user interface|user experience|multimedia|\
broadcast|radio|television|tv and radio|audio specialist|\
videographer|video editor|photographer|photography|photo lab|\
photo studio|photo finisher|graphic design|graphics?|art director|\
producer|production studio|recording studio|editor|editing|\
online assistant|cms production|reprographics|imaging)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "communications: digital and creative",

    # ------------------------------------------------------------------
    # Leadership
    # ------------------------------------------------------------------
    # Leadership: senior office leadership
    str_detect(
      x,
      "\\b(chief of staff|staff director|executive director|washington director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "leadership team: senior office leadership",

    str_detect(
      x,
      "\\b(chief of staff|chef of staff|staff director|executive director|chief operating officer|executive officer|managing director|leadership director|chief and director|cao emeritus)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "leadership team: senior office leadership",


    # Leadership: policy
    str_detect(
      x,
      "\\b(counsel)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: legal",


    #  protocol
    str_detect(
      x,
      "\\b(director of protocol|protocol director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "protocol",


    str_detect(
      x,
      "\\b(chief adviser|chief advisor|chief counsel|general counsel|special adviser|advisor to|\
chief deputy|deputy chief|chief education adviser|chief health adviser|\
senior adviser to the chairman|senior adviser to the ranking member|\
senior adviser to vice chairman|adviser to the chairman|\
adviser to chairman|adviser to conference chairman|\
adviser to the leader|adviser to the speaker|\
adviser to the congressman|senior congressional adviser|\
leadership adviser|executive adviser|executive staff adviser|\
senior special adviser|special adviser to the vice president|\
special adviser to the cao)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: adviser",


    # Leadership: district

    # ------------------------------------------------------------------
    # District, state, regional, community, and coalition work
    # ------------------------------------------------------------------

    str_detect(
      x,
      "\\b(district director|state director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "district team: district leadership",



    str_detect(
      x,
      "\\b(district director|state director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "district team: district leadership",



    # District constituencies
    # TODO search other liasons
    str_detect(
      x,
      "\\b(labor liaison|native american affairs liaison|small business coordinator|coalitions?|forest resources liaison|\
health care liaison|healthcare liaison|senior citizen liaison|\
business liaison|business relations|business coalitions|county liaison)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "district team: constituencies",

    # District outreach and field
    str_detect(
      x,
      "\\b(regional director|manhattan director|district|field|area|region|regional|community liaison|outreach|delegation|\
dayton|grand rapids|counties|co coordinator|alaska|island|juneau|kansas|\
island|central|north|northeastern|
kent|olympia|olympic|northeast|south sound|minnesota|
central|eastern|west|western|northwest|southwest|northwestern|southwestern|southeast|southwest|southern|northern|business liaison|community development|manhattan|downriver|downstate|state)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "district team: outreach and field",

    str_detect(
      x,
      "\\b(community affairs|community engagement|community relations|community|\
community representative|community coordinator|community liaison|\
community worker|community service|public engagement|\
state education liaison|state office liaison|state liaison|\
congressional representative|senator representative|\
senator s representative|senators representative|\
island representative|neighbor island representative|\
delegation representative|state representative|\
rural representative|county coordinator|co director|\
county director|community director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "district team: outreach and field",

    str_detect(
      x,
      "\\b(district|field|fieldman|fieldworker|regional|region|county|\
state office|state coordinator|state projects|state co director|\
downstate|upstate|suburban|rural|low country|lowcountry|\
manhattan|brooklyn|chicago|milwaukee|springfield|annapolis|\
dayton|newport|reno|tulsa|olympia|mankato|moorhead|molokai|\
grand rapids|new castle|newcastle|long island|south jersey|\
new jersey|new york city|florida keys|fresno|pierce county|\
king county|kitsap|chautauqua|niagara|monroe county|\
prince william|williamson county|yavapai|kent and sussex|\
gaston|cleveland county|collier county|robertson county|\
northwestern washington|northwest washington|southwest washington|\
west michigan|east bay|east king county|east river|\
north georgia|north louisiana|north county|northeast|\
southeast|southwest|south texas|south sound|west river|\
interior alaska|southeast alaska|san luis valley|santa fe|\
solano|vermont offices|oregon projects|georgia projects|\
kansas development|montana special projects|tennessee coordinator|\
rural initiatives|local relations|area representative|\
office representative|projects representative)\\b" |> str_remove_all("\n") |> str_squish()  |> str_remove_all("\n") |> str_squish()
    ) ~ "district team: outreach and field",

    # ------------------------------------------------------------------
    # Instruction and training
    # ------------------------------------------------------------------

    # Education and training
    str_detect(
      x,
      "\\b(teacher|instructor|academic|academy|instructional|
      education and training|strategic learning and development|proctor|
training and development|humanities|arts|residence|residential|training|superintendent|learning)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "instruction",


    str_detect(
      x,
      "\\b(teacher|instructor|academic|academy|instructional|\
education and training|training and development|learning and development|\
training specialist|training coordinator|training branch manager|\
training project coordinator|software training|strategic learning|\
learning specialist|test training and exercise planner|\
professional development manager)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "instruction",

    # ------------------------------------------------------------------
    # Information technology
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(advance|protocol|trip director|national trip director|travel|computer|it |\
trip planning|arrangements|inaugural coordinator)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: travel and protocol",


    str_detect(
      x,
      "\\b(information technology|info technology|i t specialist|\
it assistant|it coordinator|it manager|it request|it strategy|\
computer|software|programmer|developer|application development|\
business process applications|technology asset|technology solutions|\
technology management|technology director|house technology|\
website technology|server migration|systems?|network|\
telecommunications|fiber and wireless|wireless service|\
voice and video|infrastructure branch|data processing|\
data production|data set|data specialist|database|\
office technology|electronic procurement applications|\
electronics procurement applications|cms director|\
closed caption television|closed circuit television)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: information technology",

    # ------------------------------------------------------------------
    # Human resources, benefits, and organizational development
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(personnel|human capital|human capitol|employee benefits|personal|employee|retreat|workforce development|\
employee relations|staffing specialist|recruiter|compensation|\
organization development|organization performance|\
organization change management|diversity and organization change|\
performance and awards|transit benefits|employee assistance program|\
employee assistance specialist|eap director|workplace safety|\
health and safety professional)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: human resources",

    # ------------------------------------------------------------------
    # Finance, contracts, procurement, assets, and grants
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(comptroller|finance|financial|budget|accountant|accounting|collections|development|business innovation|paralegal|\
accounts payable|procurement|purchasing|contracts specialist|\
contracting|asset management|assets|grant specialist|^grant$|\
tax resources|internal controls|assurance and risk management|\
inventory control|inventory and planning|resource management|\
resources management|resource manager|resource specialist|banking associate|branch manager|business process|business manager|\
property assistant)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: finance and procurement",

    # ------------------------------------------------------------------
    # Emergency preparedness, continuity, and planning
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(emergency preparedness|emergency continuity|preparedness|emergency|\
continuity planning|continuity management|business continuity|\
preparedness planner|coop planning|strategic planning|\
planning director|planning manager|planning specialist|\
executive planning|director for planning)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: planning and preparedness",

    # ------------------------------------------------------------------
    # Programs, projects, events, and initiatives
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(program|projects?|special initiatives|strategic initiatives|programs|\
task force coordinator|special projects?|state projects?|\
member projects?|members projects?|federal projects?|\
development and special projects|events?|conference coordinator|\
retreat director|tour program|student loan repay|\
process improvement|process management and innovation|\
change initiative|business improvement team|\
business innovation|business innovations|quality assurance|\
options|solutions delivery)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: programs and projects",

    # ------------------------------------------------------------------
    # Scheduling, office, clerical, and executive support
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(operating|\
schedule|scheduling|calendar clark|d c schedule|office manager|office administrator|office adminstrator|data|traffic|front office|office assistant|\
office assistant|office coordinator|office staff|staff office|client|interpreter|\
front office|office managerial assistant|clerical|adminstrative assistant|\
data entry|document processing|documents manager|\
executive assistant|executive team assistant|management assistant|\
special assistant|senior assistant|congressional assistant|\
capitol assistant|home assistant|general inquiries assistant|\
assistant to|asst to rep|personal staff|personal assistant|\
secretary|executive secretariat|staff assistant|staff coordinator|\
conference coordinator|congresswomen s suite coordinator|\
member family room coordinator|members? family room coordinator|call center|
rooms coordinator|prayer room coordinator)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: office operations",

    # ------------------------------------------------------------------
    # Mail, logistics, facilities, trades, retail, and food service
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(mailroom|postmaster|postal|superintendent of mails|carpet|upholsterer|upholstery|gallery|historic|storeroom|supply|capitol|cable|fitness|laboratory|\
package delivery|inventory specialist|receiving|warehouse|\
storeroom|freight handler|fleet attendant|parking|vehicle|\
maintenance|facilities|furnishings?|furniture|cabinet|cabinetmaker|\
carpet|drapemaker|upholster|finisher|locksmith|engraver|framer|\
cable installer|mechanic helper|laborer|custodial|\
gift shop|sales associate|sales specialist|vending manager|\
food manager|barber|hairstylist|shoe shine|textile|retail|\
office supply|stationery|printer|printing|property|process|\
door attendant|garage attendant|food manager|gift shop|shift)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: facilities and logistics",

    # ------------------------------------------------------------------
    # Reporting, transcription, interpretation, and captioning
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(court reporter|official reporter|reporter of debates|reporter|\
chief reporter|office reporter|transcriber|captioner|captioning|\
interpreter|sign language interpreter|hearing clark|\
hearing coordinator|hearings coordinator)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: reporting and accessibility",

    # ------------------------------------------------------------------
    # Archives, library, museum, and collections
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(archival|archives|archivist|archiving|cataloger|collection|\
collections?|registrar|curatorial|curator|museum|\
historic preservation|historical publications|oral history|\
library assistant|library automation|library science|\
reference assistant|conservator|bookbinder)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: archives and collections",

    # ------------------------------------------------------------------
    # Security, chamber, and institutional support
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(security|police|plainclothesman|captain|lieutenant|door|safety|\
private first|private class|private with training|detective|\
fingerprint personnel|identification specialist|sergeant|\
doorkeeper|doorkeepers|chamber attendant|chamber manager|\
cloakroom|escort and volunteer|superintendent of doors|\
vehicle and materials screener|vehicle and maintenance screener|\
special agent)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: security and chamber support",

    # ------------------------------------------------------------------
    # Customer, member, and institutional services
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(customer relations|customer service|customer solutions|customer|
client relations|member and guest relations|member service|\
member services|sales and customer service|capitol exchange|tour|
cvc coordinator|service director|support services)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: support services",

    str_detect(
      x,
      "\\b(legal adviser|legal assistant|legal affairs|legal extern|\
paralegal|litigation|contracts paralegal|law assistant|\
education and workforce counsels)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: legal",

    # OLD CODING
    # Information technology and systems
    str_detect(
      x,
      "\\b(computer specialist|systems?|telecommunications services|systems development services|network|information technology|it director|database|business continuity)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: information technology",

    # Programs and projects
    str_detect(
      x,
      "\\b(program manager|senior program manager|project assistant|project director|project manager|projects assistant|projects director|special projects coordinator|director of special projects)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: programs and projects",

    # Mailroom and inventory
    str_detect(
      x,
      "\\b(mailroom assistant|inventory specialist|manager textiles|laborer)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: facilities and logistics",

    # Administrative office operations
    str_detect(
      x,
      "\\b(chief administrative officer|administrative director|administrator|administrator assistant|administration|information|scheduling|accounts|payable|acquisitions?|appointments?|administrative assistant|office manager|office administrator|office director|account|accountant|accounting|enterprise|retail|logistics|equipment|technical|grants|professional assistant|operations? director|scheduler|scheduling director|executive assistant|trip coordinator|vendor|visitor|compensation|stationery|staff assistant|receptionist|systems administrator|captioning|workflow|office coordinator|personal assistant|secretary|printing|pay|staff professional|contract|audit|contracting|records?|payroll|finance|financial|budget|human resources?|d c office|member services|group assistant|support|contractor|engineering|engineer|desk|administrative|operations)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: office operations",

    # Generic special assistants and congressional assistants
    str_detect(
      x,
      "\\b(special assistant|congressional assistant|advance assistant)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: executive and staff support",

    # Support services
    str_detect(
      x,
      "\\b(social worker|driver|chauffeur|security|facilities|mail|messenger|aide|police|doorkeeper|operator|foreman|journeyman|cloakroom|special events|assistant to|assist to|child care|credentialing|counselor|services|technician|supervisor)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: support services",

    # Special institutional roles
    str_detect(
      x,
      "\\b(chaplain|clerk|floor|sergeant|private first class|apprentice|archival|archives|archivist|conservator|bookbinder|curator|guide|tours|parliamentarian|historian|social worker)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: other",

    # ------------------------------------------------------------------
    # Protocol and other special institutional roles
    # ------------------------------------------------------------------
    str_detect(
      x,
      "\\b(chaplain|clerk|floor|parliamentarian|historian|\
chief of protocol|director for protocol|protocol officer|\
protocol assistant|protocol director|chief of protocol and foreign travel|\
tour coordinator|guide|tours)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "administrative: other",

    # ------------------------------------------------------------------
    # Caucus, coalition, party, and political roles
    # ------------------------------------------------------------------

    str_detect(
      x,
      "\\b(majority|minority|caucus|whip|leadership adviser|leadership director|leadership liaison)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "party: leadership",


    str_detect(
      x,
      "\\b(democratic|republican|bipartisan)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "party: other",



    # political director|\
    # departmental political director|caucus|coalition director|\
    # coalitions director|coalitions coordinator|coalitions adviser|\
    # coalitions assistant|equality caucus|pro life caucus|\
    # congressional black caucus|congressional hispanic caucus|\
    # new democrat coalition|conservative coalitions|lgbt caucus|\
    # professional life caucus

    # ------------------------------------------------------------------
    # Shared staff
    # ------------------------------------------------------------------
    #     str_detect(
    # x,
    # "\\b(shared staff|shared staffer|shared associate staffer|\
    # staff shared|shared employed|shared employee|\
    # shared congressional progressive caucus staff)\\b" |> str_remove_all("\n") |> str_squish()
    #     ) ~ "unclear: shared staff",
    #
    # ------------------------------------------------------------------
    # Generic adviser titles without an informative portfolio
    # ------------------------------------------------------------------

    # Unclear generic titles

    str_detect(
      x,
      "^(staff shared|shared staff full committee|shared employee|shared staffer|shared associate staffer|shared employed)$"
    ) ~ "unclear: shared staff",

    str_detect(
      x,
      "^(adviser|adviser at large|senior adviser|special adviser|\
chief adviser|senator adviser|staff adviser|senior adviser( [a-z]+| [0-9]+)+)$"
    ) ~ "policy: adviser",

    str_detect(
      x,
      "^(private|private first)$"
    ) ~ "unclear: military",


    str_detect(
      x,
      "^(office|program|staff|director|assistant director|deputy director|acting director|acting associate director|acting manager|no title listed|no title listed                             |
|not listed)$"
    ) ~ "unclear: generic title",


    ###################################################################################
    # ------------------------------------------------------------------
    # Generic staff and management labels
    # ------------------------------------------------------------------

    # Generic staff labels
    str_detect(
      x,
      "\\b(senior staff associate|staff associate|professional staff|professional staff member|senior professional staff)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "policy: professional committee staff",

#     str_detect(
#       x,
#       "^(assistant|associate|associate coordinator|associate director|\
# assistant director|deputy|deputy assistant director|deputy director|\
# director|co director|director designate|manager|management|\
# assistant manager|acting manager|branch chief|branch manager|chief|\
# senior deputy|senior director|team leader|team leader designee|\
# shift leader|principal|first assistant|second assistant|third assistant|\
# staff|staffer|staff member|staff associate|senior staff|\
# senior staff member|associate staff|associate staff member|\
# assistant staff|assistant staffer|official staff|permanent staff|\
# congressional staff|congressional staffer|senate staff|\
# professional director|professional member|staff manager|\
# staff specialist|staff facilitator|associate registrar|registrar|\
# representative|representative at large)$"
#     ) ~ "unclear: generic title",

    # shared staff
    str_detect(
      x,
      "\\b(shared staff)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "unclear: shared staff",


    # Staff representative labels that only identify a member
    str_detect(
      x,
      "\\b(associate staff rep|staff rep|assistant staff)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "unclear: staff",




    # Acting/director acronyms without an interpretable portfolio
    # TODO classify these
    str_detect(
      x,
      "\\b(acting director first call|acting director hosc|acting director osc|\
acting eap director|director evs|cms director|ocs director|\
deputy director ccsc io|manager smi|y2k deputy director)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "unclear: organizational acronym",

    # Designees and other ambiguous titles
    str_detect(
      x,
      "\\b(designee to the chairman|chair s designee|chairman s designee|\
acting designee|director designate)\\b" |> str_remove_all("\n") |> str_squish()
    ) ~ "unclear: designee",

    TRUE ~ NA_character_
  )
}
# ```

### Important ordering choices
#
# - Topic-specific rules precede broad adviser rules, so `energy adviser`, `senior tax adviser`, and `health adviser` become policy roles.
# - Constituent rules precede communications, so `constituent correspondence coordinator` remains constituent casework rather than general correspondence.
# - Geographic and community rules precede generic `director`, `representative`, and `liaison` rules.
# - Temporary rules occur near the top, so `assistant food manager temporary` and `associate staff part time` are temporary regardless of their substantive title.
# - Generic labels such as `assistant`, `director`, `manager`, `liaison`, and `representative` are marked unclear rather than assigned based only on rank.
# - Date-appended `senior adviser` records are classified as generic advisers. If those strings represent payments rather than job titles, move them into `"other expenses"` instead.


# ```
#
# ## First-title priority with second-title fallback
#
# ```r
staff_classified <- titles_clean %>%
  mutate(
    classification_1 = classify_title(title_1),
    classification_2 = classify_title(title_2),

    # Use title 2 only if title 1 did not produce a classification.
    classification = coalesce(classification_1, classification_2),

    category = str_remove(classification, ":.*$"),
    subcategory = str_remove(classification, "^[^:]+:\\s*")
  )
# ```
#
# For example:
#
#   - `"Legislative Assistant / Communications Director"` is classified from `"Legislative Assistant"`.
# - `"Assistant to the Member / Legislative Correspondent"` falls back to the second title if `"Assistant to the Member"` is left unclassified.
# - `"Staff Assistant / Caseworker"` is classified as administrative because the first title is informative. If casework should override generic administrative titles, put casework rules into a separate pair-level priority step instead.
#
# ## Audit unmatched and conflicting values
#
# These checks will be useful before expanding the rule set:
#
#   ```r

###################################################################
staff_classified %>%
  count(title_1, classification_1, sort = T)|>
  #slice_sample(n = 100) |>
  head(20) |>
  knitr::kable()

# Rows where neither halves are recognized
staff_classified %>%
  filter(
    is.na(classification_1) & is.na(classification_2)
  ) %>%
  count(title_1,
        #title_2,
        sort = TRUE) |>
  select(-n) |>
  #head(100) |>
  knitr::kable()


# # Titles that remain unclassified
staff_classified %>%
  filter(is.na(classification)) %>%
  count(title_1, title_2, sort = TRUE) |>
  head(100) |>
  knitr::kable()


# Rows where both halves are recognized but imply different categories
staff_classified %>%
  filter(
    !is.na(classification_1),
    !is.na(classification_2),
    str_remove(classification_1, ":.*$") !=
      str_remove(classification_2, ":.*$")
  ) %>%
  count(title_1, title_2, classification_1, classification_2, sort = TRUE)

# Overall distribution
staff_classified %>%
  count(category, subcategory, sort = TRUE)
# ```
#
# One important modeling choice is whether broad positions such as `"Legislative Director"` belong under **leadership team** or **legislative**. In the sample function, it is leadership because that rule appears first. Move it below the legislative section if every title containing “legislative” must instead be categorized as legislative.
