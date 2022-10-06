#' @include OBF.R
#' @include POC.R
NULL

GSD_OBF <- function(n,norep,K,RP,approach) {
  switch (RP,
          'RAR' = switch(approach,
                         '1' = switch(K,
                                      '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                      '2' = OBF_2_RAR_A1(n,norep),
                                      '3' = OBF_3_RAR_A1(n,norep),
                                      '4' = OBF_4_RAR_A1(n,norep),
                                      "GSD with only 2,3, or 4 stages are supported at this moment"
                         ),
                         '2' = switch(K,
                                      '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                      '2' = OBF_2_RAR_A2(n,norep),
                                      '3' = OBF_3_RAR_A2(n,norep),
                                      '4' = OBF_4_RAR_A2(n,norep),
                                      "GSD with only 2,3, or 4 stages are supported at this moment"
                         )),
          'PBD2' = switch(K,
                          '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                          '2' = OBF_2_PBD2(n,norep),
                          '3' = OBF_3_PBD2(n,norep),
                          '4' = OBF_4_PBD2(n,norep),
                          "GSD with only 2,3, or 4 stages are supported at this moment"
          ),
          'BSD'=switch(approach,
                       '1' = switch(K,
                                    '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                    '2' = OBF_2_BSD_A1(n,norep),
                                    '3' = OBF_3_BSD_A1(n,norep),
                                    '4' = OBF_4_BSD_A1(n,norep),
                                    "GSD with only 2,3, or 4 stages are supported at this moment"
                       ),
                       '2' = switch(K,
                                    '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                    '2' = OBF_2_BSD_A2(n,norep),
                                    '3' = OBF_3_BSD_A2(n,norep),
                                    '4' = OBF_4_BSD_A2(n,norep),
                                    "GSD with only 2,3, or 4 stages are supported at this moment"
                       )
                       
          ),
          'EBC' = switch(K,
                         '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                         '2' = OBF_2_EBC(n,norep),
                         '3' = OBF_3_EBC(n,norep),
                         '4' = OBF_4_EBC(n,norep),
                         "GSD with only 2,3, or 4 stages are supported at this moment"
          ),
          'CHEN'=switch(approach,
                        '1' = switch(K,
                                     '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                     '2' = OBF_2_CHEN_A1(n,norep),
                                     '3' = OBF_3_CHEN_A1(n,norep),
                                     '4' = OBF_4_CHEN_A1(n,norep),
                                     "GSD with only 2,3, or 4 stages are supported at this moment"
                        ),
                        '2' = switch(K,
                                     '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                     '2' = OBF_2_CHEN_A2(n,norep),
                                     '3' = OBF_3_CHEN_A2(n,norep),
                                     '4' = OBF_4_CHEN_A2(n,norep),
                                     "GSD with only 2,3, or 4 stages are supported at this moment"
                        )
                        
          ),
          "GSD with just RAR, CHEN, EBC, BSD and PBD(2) is supported at this moment"
  )
}

GSD_POC <- function(n,norep,K,RP,approach) {
  switch (RP,
          'RAR' = switch(approach,
                         '1' = switch(K,
                                      '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                      '2' = POC_2_RAR_A1(n,norep),
                                      '3' = POC_3_RAR_A1(n,norep),
                                      '4' = POC_4_RAR_A1(n,norep),
                                      "GSD with only 2,3, or 4 stages are supported at this moment"
                         ),
                         '2' = switch(K,
                                      '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                      '2' = POC_2_RAR_A2(n,norep),
                                      '3' = POC_3_RAR_A2(n,norep),
                                      '4' = POC_4_RAR_A2(n,norep),
                                      "GSD with only 2,3, or 4 stages are supported at this moment"
                         )
          ),
          'PBD2' = switch(K,
                          '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                          '2' = POC_2_PBD2(n,norep),
                          '3' = POC_3_PBD2(n,norep),
                          '4' = POC_4_PBD2(n,norep),
                          "GSD with only 2,3, or 4 stages are supported at this moment"
          ),
          'BSD'=switch(approach,
                       '1' = switch(K,
                                    '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                    '2' = POC_2_BSD_A1(n,norep),
                                    '3' = POC_3_BSD_A1(n,norep),
                                    '4' = POC_4_BSD_A1(n,norep),
                                    "GSD with only 2,3, or 4 stages are supported at this moment"
                       ),
                       '2' = switch(K,
                                    '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                    '2' = POC_2_BSD_A2(n,norep),
                                    '3' = POC_3_BSD_A2(n,norep),
                                    '4' = POC_4_BSD_A2(n,norep),
                                    "GSD with only 2,3, or 4 stages are supported at this moment"
                       )
                       
          ),
          'EBC' = switch(K,
                         '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                         '2' = POC_2_EBC(n,norep),
                         '3' = POC_3_EBC(n,norep),
                         '4' = POC_4_EBC(n,norep),
                         "GSD with only 2,3, or 4 stages are supported at this moment"
          ),
          'CHEN'=switch(approach,
                        '1' = switch(K,
                                     '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                     '2' = POC_2_CHEN_A1(n,norep),
                                     '3' = POC_3_CHEN_A1(n,norep),
                                     '4' = POC_4_CHEN_A1(n,norep),
                                     "GSD with only 2,3, or 4 stages are supported at this moment"
                        ),
                        '2' = switch(K,
                                     '1' = print("GSD with only 2,3, or 4 stages are supported at this moment"),
                                     '2' = POC_2_CHEN_A2(n,norep),
                                     '3' = POC_3_CHEN_A2(n,norep),
                                     '4' = POC_4_CHEN_A2(n,norep),
                                     "GSD with only 2,3, or 4 stages are supported at this moment"
                        )
                        
          ),
          "GSD with just RAR, CHEN, EBC, BSD and PBD(2) is supported at this moment"
  )
}
