-- let e1 = ELam "f" (ELam "xs" (EIf (EBin Eq (EVar "xs") ENil) ENil (ELet "h" (EApp (EVar "head") ( EVar "xs")) (ELet "t" (EApp (EVar "tail") (EVar "xs")) (EBin Cons (EApp (EVar "f") (EVar "h")) (EApp (EApp (EVar "map") (EVar "f")) (EVar "t")))))))

-- (
--    ELam "f" 
--    (
--            ELam "xs" 
--            (
--                     EIf 
--                     (
--                          EBin Eq 
--                          (
--                                  EVar "xs"
--                          ) 
--                          ENil
--                     ) 
--                     ENil 
--                     (
--                          ELet "h" 
--                          (
--                                  EApp 
--                                  (
--                                        EVar "head"
--                                  ) 
--                                  (
--                                     EVar "xs"
--                                  )
--                          ) 
--                          (
--                                  ELet "t" 
--                                  (
--                                          EApp 
--                                          (
--                                               EVar "tail"
--                                          ) 
--                                          (
--                                               EVar "xs"
--                                          )
--                                  ) 
--                                  (
--                                          EBin Cons 
--                                          (
--                                               EApp 
--                                               (
--                                                      EVar "f"
--                                               ) 
--                                               (
--                                                      EVar "h"
--                                               )
--                                          ) 
--                                          (
--                                               EApp 
--                                               (
--                                                      EApp 
--                                                      (
--                                                             EVar "map"
--                                                      ) 
--                                                      (
--                                                             EVar "f"
--                                                      )
--                                               ) 
--                                               (
--                                                      EVar "t"
--                                               )
--                                          )
--                                   )
--                           )
--                     )
--             )
--      )
-- ) 