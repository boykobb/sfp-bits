data Day = DMY (Dn,Month,Year) | MDY (Month,Dn,Year)
         | DM (Dn,Month) | MD (Month,Dn) | WD Wd | Today | Yest | Tomr
type Dn = Int
type Year = Int
data Month = Mn Mname | Mi Int
data Mname = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec  deriving (Show)
data Wd = Mon | Tue | Wed | Thu | Fri | Sat | Sun  deriving (Show)

instance Show Month where
  show (Mn m) = show m
  show (Mi m) = show m

instance Show Day where
  show Today = "today"
  show Tomr = "tomorrow"
  show Yest = "yesterday"
  show (WD d) = show d
  show (MD (m,d)) = show m ++ " " ++ show d
  show (DM (d,m)) = show d ++ " " ++ show m
  show (MDY (m,d,y)) = show m ++ dm1 ++ show d ++ dm2 ++ show y where
    dm1 = case m of {Mn m -> " "; Mi m -> "/"}
    dm2 = case m of {Mn m -> ", "; Mi m -> "/"}
  show (DMY (d,m,y)) = show d ++ dlm ++ show m ++ dlm ++ show y where
    dlm = case m of {Mn m -> " "; Mi m -> "/"}

dates = [DMY (12, Mn Mar, 2020), MDY (Mn Nov, 23, 2016), MD (Mn Feb, 5), DMY (23, Mi 5, 2022)]
dxfrm (MDY(m,d,y)) = DMY(d,m,y)
dxfrm (MD(m,d))    = DMY(d,m,2023)
dxfrm day          = day
d = map dxfrm dates
