% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_conversion.R
\name{mixed_mode_s_params_3}
\alias{mixed_mode_s_params_3}
\alias{differential_gamma_3_port}
\alias{differential_gain_3_port}
\alias{differential_reverse_gain_3_port}
\alias{common_gain_3_port}
\alias{common_reverse_gain_3_port}
\alias{cmrr}
\alias{imbalance}
\title{3-Port Mixed-Mode Parameters}
\usage{
differential_gamma_3_port(S22, S23, S32, S33)

differential_gain_3_port(S21, S31)

differential_reverse_gain_3_port(S12, S13)

common_gain_3_port(S21, S31)

common_reverse_gain_3_port(S12, S13)

cmrr(S21d, S21c)

imbalance(S21, S31)
}
\arguments{
\item{S22}{Single-ended reflection on positive balanced port}

\item{S23}{Single-ended reverse isolation between balanced ports}

\item{S32}{Single-ended isolation between balanced ports}

\item{S33}{Single-ended reflection on negative balanced port}

\item{S21}{Single-ended gain from unbalanced port to positive balanced port}

\item{S31}{Single-ended gain from unbalanced port to negative balanced port}

\item{S12}{Single-ended reverse gain from unbalanced port to positive balanced port}

\item{S13}{Single-ended reverse gain from unbalanced port to negative balanced port}

\item{S21d}{Differential-mode gain}

\item{S21c}{Common-mode gain}
}
\description{
Converts single-ended S parameters to differential.  Functions include:
\describe{
\item{differential_gamma_3_port}{Differential-mode reflection cofficient}
\item{differential_gain_3_port}{Differential-mode insertion gain}
\item{differential_reverse_gain_3_port}{Differential-mode reverse insertion gain}
\item{common_gain_3_port}{Common-mode insertion gain}
\item{common_reverse_gain_3_port}{Common-mode reverse insertion gain}
\item{cmrr}{Common-mode rejection ratio}
\item{imbalance}{Imbalance between single-ended and differential ports}
}
Assumes port layout:
\enumerate{
\item Single-Ended Port
\item Balanced (Positive)
\item Balanced (Negative)
}
}
\examples{
df <- read_snp(rftk_example("Balun.s3p"), numeric_format = "MA")
df <- dplyr::select(df, -ang)
df <- tidyr::pivot_wider(df, names_from = "parameter", values_from = "mag")
dplyr::mutate(df,
              S21ds = differential_gain_3_port(S21, S31),
              S21cs = common_gain_3_port(S21, S31),
              S12ds = differential_reverse_gain_3_port(S12, S13),
              S12cs = common_reverse_gain_3_port(S12, S13),
              S22dd = differential_gamma_3_port(S22, S23, S32, S33),
              cmrr = cmrr(S21ds, S21cs),
              imbalance = imbalance(S21, S31))

}
