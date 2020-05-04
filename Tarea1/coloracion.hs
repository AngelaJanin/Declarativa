data Color = Rojo | Amarillo | Verde | Azul deriving (Eq, Show)

data Balcanes = Albania
                | Bulgaria
                | BosniayHerzegovina
                | Kosovo
                | Macedonia
                | Montenegro
            deriving(Eq, Show)

type Ady = [(Balcanes, Balcanes)]

adyacencias :: Ady
adyacencias = [(Albania, Montenegro ), (Albania, Kosovo), (Albania, Macedonia), (Bulgaria, Macedonia), (BosniayHerzegovina, Montenegro), (Kosovo, Macedonia), (Kosovo, Montenegro)]   

esBueno::Ady -> Coloracion -> Bool 
esBueno = 