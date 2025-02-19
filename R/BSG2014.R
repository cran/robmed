# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Business simulation game data
#'
#' @description
#' The data were collected from 354 senior business administration students
#' during a business simulation game at a Western European University.
#'
#' The game was played for a total of 12 rounds (i.e., two separate games of 6
#' rounds) as part of the capstone strategy class.  Students were randomly
#' assigned to teams of four, and surveyed in three waves: prior to the first
#' game, in between the two games, and after the second game (with different
#' variables being surveyed in the different waves).
#'
#' The 354 students formed 92 teams, and the responses of individual students
#' were aggregated to the team level.  Leaving out teams with less than 50
#' percent response rate yields \eqn{n = 89} teams.  Only a small subset of the
#' collected variables are included here.
#'
#' @usage
#' data("BSG2014")
#'
#' @format
#' A data frame with 89 observations on the following 13 variables.
#' \describe{
#'
#'   \item{\code{ProcessConflict}}{Based on Shah & Jehn (1993), the team
#'   members rated three items on the presence of conflict related to the
#'   process of working together, using a 5-point scale (1 = none, 5 = a lot).
#'   The individual responses were aggregated by taking the average across
#'   items and team members.  Process conflict was measured in the second
#'   survey (between the two games).}
#'
#'   \item{\code{SharedExperience}}{As teams were randomly formed, no prior
#'   shared group experience is expected, and shared group experience and
#'   training is developed during the first game for the second game.  Hence
#'   the team performance score on the first game is used as a proxy for the
#'   level of shared group experience and training.  Those scores were computed
#'   through a mix of five objective performance measures: return on equity,
#'   earnings-per-share, stock price, credit rating, and image rating.  The
#'   computation of the scores is handled by the simulation game software, and
#'   details can be found in Mathieu & Rapp (2009).  The scores ranged from 57
#'   to 111, and they were communicated to the teams only after the third
#'   survey.}
#'
#'   \item{\code{TaskConflict}}{Using the intra-group conflict scale of Jehn
#'   (1995), the team members rated four items on the presence of conflict
#'   regarding the work on a 5-point scale (1 = none, 5 = a lot).
#'   The individual responses were aggregated by taking the average across
#'   items and team members.  Task conflict was measured in the second survey
#'   (between the two games).}
#'
#'   \item{\code{TeamCommitment}}{The team members indicated the extent to
#'   which they agree or disagree with four items on commitment to the team,
#'   which are based on Mowday, Steers & Porter (1979), using a 5-point scale
#'   (1 = strongly disagree, 5 = strongly agree).  The individual responses
#'   were aggregated by taking the average across items and team members.
#'   Team commitment was measured in the third survey (after the second game).}
#'
#'   \item{\code{TeamPerformance}}{Following Hackman (1986), the team members
#'   indicated the extent to which they agree or disagree with four items on
#'   the team's functioning, using a 5-point scale (1 = strongly disagree, 5 =
#'   strongly agree).  The individual responses were aggregated by taking the
#'   average across items and team members.  Subjective team performance was
#'   measured in the third survey (after the second game).}
#'
#'   \item{\code{TMS}}{Transactive memory systems (TMS) are defined as shared
#'   systems that people in relationships develop for encoding, storing, and
#'   retrieving information about different substantive domains.  TMS was
#'   operationalized with Lewis’ (2003) 15-item scale that measures the three
#'   sub-dimensions of TMS (specialization, credibility, and coordination).
#'   For each item, the team members responded on a 5-point scale (1 = strongly
#'   disagree, 5 = strongly agree).  Following Lewis (2003), the three sub
#'   dimensions were aggregated to form the TMS construct.  That is, the
#'   individual responses were aggregated by taking the average across all 15
#'   items and team members.  TMS was measured in the second survey (between
#'   the two games).}
#'
#'   \item{\code{ValueDiversity}}{Using the short Schwartz’s value survey
#'   (Lindeman & Verkasalo, 2005), the team members rated ten items on the
#'   importance of certain values (1 = not important, 10 = highly important).
#'   For each value item, the coefficient of variation of the individual
#'   responses across team members was computed, and the resulting coefficients
#'   of variation were averaged across the value items.  Value diversity was
#'   measured in the first survey (before the first game).}
#'
#'   \item{\code{ProceduralJustice}}{Based on the intra-unit procedural justice
#'   climate scale of Li & Cropanzano (2009), the team members indicated the
#'   extent to which they agree or disagree with four items on a 5-point scale
#'   (1 = strongly disagree, 5 = strongly agree).  The individual responses
#'   were aggregated by taking the average across items and team members.
#'   Procedural justice was measured in the third survey (after the second
#'   game).}
#'
#'   \item{\code{InteractionalJustice}}{Using the intra-unit interactional
#'   justice climate scale of Li & Cropanzano (2009), the team members
#'   indicated the extent to which they agree or disagree with four items on a
#'   5-point scale (1 = strongly disagree, 5 = strongly agree).  The individual
#'   responses were aggregated by taking the average across items and team
#'   members.  Interactional justice was measured in the third survey (after
#'   the second game).}
#'
#'   \item{\code{SharedLeadership}}{Following Carson, Tesluk & Marrone (2007),
#'   every team member assessed each of their peers on the question of
#'   \sQuote{To what degree does your team rely on this individual for
#'   leadership?} using a 5-point scale (1 = not at all, 5 = to a very large
#'   extent).  The leadership ratings were aggregated by taking the sum and
#'   dividing it by the number of pairwise relationships among team members.
#'   Shared leadership was measured in the second survey (between the two
#'   games).}
#'
#'   \item{\code{AgeDiversity}}{Following Harrison & Klein (2007), age
#'   diversity was operationalized by the coefficient of variation of the
#'   team members' ages.}
#'
#'   \item{\code{GenderDiversity}}{Gender diversity was measured with Blau's
#'   index, \eqn{1 - \sum_{j} p_{j}^{2}}{1 - the sum of the squared values of
#'   p_j}, where \eqn{p_{j}}{p_j} is the proportion of team members in the
#'   \eqn{j}{j}-th category (Blau, 1977).}
#'
#'   \item{\code{TeamScore}}{The team performance scores on the second game
#'   were computed at the end of the simulation through a mix of five objective
#'   performance measures: return on equity, earnings-per-share, stock price,
#'   credit rating, and image rating. The computation of the scores is handled
#'   by the simulation game software, and details can be found in Mathieu &
#'   Rapp (2009).  The scores ranged from 49 to 110, and they were communicated
#'   to the teams only after the third survey.}
#'
#' }
#'
#' @source The data were collected and provided by Nufer Y. Ates
#' (\url{https://orcid.org/0000-0003-4572-4101}).
#'
#' @references
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022a) A Robust Bootstrap Test
#' for Mediation Analysis.  \emph{Organizational Research Methods},
#' \bold{25}(3), 591--617.  doi:10.1177/1094428121999096.
#'
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022b) Robust Mediation Analysis:
#' The \R Package \pkg{robmed}.  \emph{Journal of Statistical Software},
#' \bold{103}(13), 1--45.  doi:10.18637/jss.v103.i13.
#'
#' Blau, P.M. (1977) \emph{Inequality and Heterogeneity: A Primitive Theory of
#' Social Structure}. New York, NY: Free Press.
#'
#' Carson, J.B., Tesluk, P.E. and Marrone, J.A. (2007) Shared Leadership in
#' Teams: An Investigation of Antecedent Conditions and Performance.
#' \emph{Academy of Management Journal}, \bold{50}(5), 1217--1234.
#' doi:10.5465/amj.2007.20159921.
#'
#' Hackman, J.R. (1986) The Psychology of Self-Management in Organizations.
#' In Pallack, M.S and Perloff, R.O. (Eds.), \emph{Psychology and Work:
#' Productivity, Change, and Employment}, 89--136.  Washington, DC: American
#' Psychological Association.
#'
#' Harrison, D.A. and Klein, K.J. (2007) What's the Difference? Diversity
#' Constructs as Separation, Variety, or Disparity in Organizations.
#' \emph{Academy of Management Review}, \bold{32}(4): 1199--1228.
#' doi:10.5465/amr.2007.26586096.
#'
#' Jehn, K.A. (1995) A Multimethod Examination of the Benefits and Detriments
#' of Intragroup Conflict.  \emph{Administrative Science Quarterly},
#' \bold{40}(2), 256--285.  doi:10.2307/2393638.
#'
#' Lewis, K. (2003) Measuring Transactive Memory Systems in the Field: Scale
#' Development and Validation.  \emph{Journal of Applied Psychology},
#' \bold{88}(4), 587--604.  doi:10.1037/0021-9010.88.4.587.
#'
#' Li, A. and Cropanzano, R. (2009) Fairness at the Group Level: Justice
#' Climate and Intraunit Justice Climate.  \emph{Journal of Management},
#' \bold{35}(3), 564--599.  doi:10.1177/0149206308330557.
#'
#' Lindeman, M. and Verkasalo, M. (2005) Measuring Values With the Short
#' Schwartz's Value Survey.  \emph{Journal of Personality Assessment},
#' \bold{85}(2), 170--178.  doi:10.1207/s15327752jpa8502_09.
#'
#' Mathieu, J.E. and Rapp, T.L. (2009).  Laying the Foundation for Successful
#' Team Performance Trajectories: The Roles of Team Charters and Performance
#' Strategies.  \emph{Journal of Applied Psychology}, \bold{94}(1), 90--103.
#' doi:10.1037/a0013257.
#'
#' Mowday, R.T., Steers, R.M. and Porter, L.W. (1979) The Measurement of
#' Organizational Commitment.  \emph{Journal of Vocational Behavior},
#' \bold{14}(2), 224--247.  doi:10.1016/0001-8791(79)90072-1.
#'
#' Shah, P.P. and Jehn, K.A. (1993) Do Friends Perform Better than
#' Acquaintances? The Interaction of Friendship, Conflict, and Task.
#' \emph{Group Decision and Negotiation}, \bold{2}(2), 149--165.
#' doi:10.1007/bf01884769.
#'
#' @examples
#' data("BSG2014")
#' summary(BSG2014)
#'
#' # scatterplot matrix for the variables included in the
#' # illustrative mediation analysis of Alfons et al. (2022a)
#' x <- "ValueDiversity"
#' y <- "TeamCommitment"
#' m <- "TaskConflict"
#' plot(BSG2014[, c(x, y, m)], pch = 21, bg = "black")

"BSG2014"
