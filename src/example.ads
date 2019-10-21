package Example with SPARK_Mode is

   Total : Integer := 0;

   procedure Increment (X : Integer)
     with
       Global  => (In_Out => Total),
       Depends => (Total => (X, Total)),
       Pre     => X > 0 and then Total < Integer'Last - X,
       Post    => Total = Total'Old + X;

   type T_Table is array (Positive range <>) of Natural;

   procedure Update (Table : in out T_Table;
                     Pos_1 : in Positive;
                     Pos_2 : in Positive;
                     Value : in Natural)
     -- Actualiza con el valor Value todos los elementos de la tabla que
     -- estan entre las posiciones Pos_1 y Pos_2 (ambas incluidas).
     with
       Global  => null,
       Depends => (Table => (Table, Pos_1, Pos_2, Value)),
       Pre     => Pos_1 in Table'Range and then Pos_2 in Table'Range and then
                  Pos_1 < Pos_2 and then Pos_2 < Positive'Last,
     Post    => ((for all K in Pos_1..Pos_2 => Table(K) = Value) and then
                   (for all K in Table'Range =>
                   (if K < Pos_1 or K > Pos_2 then Table(K) = Table'Old(K))));

   Resultado : Integer := 0;

   procedure Add (V : Integer; Max : Positive)
 -- Cuando el valor de V, que debe ser positivo, es menor que Max, suma V al
 -- valor acumulado en Resultado; en caso contrario suma 1 a resultado.
   with
       Global  => (In_Out => Resultado),
       Depends => (Resultado => (V, Max, Resultado)),
       Pre     => V > 0 and then Resultado < Integer'Last - V,
       Post    => (if V < Max then Resultado = Resultado'Old + V else
                     Resultado = Resultado'Old + 1);

   function Copy (Table : T_Table;
                  Min : Positive;
                  Max : Positive;
                  Value : Natural) return T_Table
   -- Retorna una copia de Table en la que todos los elementos con valores
   -- entre Min y Max se han sustituido por Value
     with
       Global  => null,
       Depends => (Copy'Result => (Table, Min, Max, Value)),
       Pre     => Min < Max and then Max < Positive'Last and then Min in Table'Range
                  and then Max in Table'Range,
       Post    => ((for all K in Min..Max => Copy'Result(K) = Value) and then
                (for all K in Copy'Result'Range =>
                     (if K < Min or K > Max then Copy'Result(K) = Table(K))));

   procedure Resta (A : Integer; B : Integer; R : out Integer)
     with
       Global  => null,
       Depends => (R => (A, B)),
       Pre     => (if A >= 0 and B <= 0 then A < Integer'Last + B) and then
                  (if A <= 0 and B >= 0 then A > Integer'First + B),
       Post    => R = A - B;

   procedure Doble (Table: in out T_Table)
     with
       Global  => null,
       Depends => (Table => (Table)),
       Pre     => (for all K in Table'Range => Table(K) <= Natural'Last / 2)
                  and then Table'Last < Positive'Last and then Table'Length > 0,
       Post    => (for all K in Table'Range => Table(K) <= Natural'Last);

end Example;
