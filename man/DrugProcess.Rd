% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocesssing.r
\name{DrugProcess}
\alias{DrugProcess}
\title{Pre-processing for DRUG}
\usage{
DrugProcess(
  exelFilePath,
  sheetName = NULL,
  drugData = NULL,
  drugCode,
  drugName,
  clinicalDrugcode,
  drugDosage,
  drugDosageUnit,
  previousConceptCode
)
}
\arguments{
\item{exelFilePath}{file path for the excel file.}

\item{sheetName}{A sheet name of interest. Sheet name for drug is usally NULL (default is NULL)}

\item{drugData}{Prepared material data. Default is NULL.}

\item{drugCode}{A column name for drug EDI code.}

\item{drugName}{A column name for drug}

\item{clinicalDrugcode}{A column name for clinical drug code}

\item{drugDosage}{A column name for drug dosage.}

\item{drugDosageUnit}{A column name for drug dosage unit.}

\item{previousConceptCode}{A column name for previous code}
}
\description{
Pre-processing for DRUG
}
\details{
Pre-processing for the Excel EDI file
}
