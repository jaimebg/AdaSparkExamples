package body Example with SPARK_Mode is

   procedure Increment (X: in Integer) is
   begin
      Total := Total + X;
   end Increment;

   procedure Update (Table : in out T_Table;
                     Pos_1 : in Positive;
                     Pos_2 : in Positive;
                     Value : in Natural) is
      J : Positive := Pos_1;
   begin
      while J <= Pos_2 loop
         Table(J) := Value;

         pragma Loop_Variant (Increases => J);
         pragma Loop_Invariant (J in Pos_1..Pos_2);
         pragma Loop_Invariant (Pos_1 in Table'Range);
         pragma Loop_Invariant (Pos_2 in Table'Range);
         pragma Loop_Invariant (for all K in Pos_1..J =>
                                  Table(K) = Value);
         pragma Loop_Invariant (for all K in Table'Range =>
                                  (if K < Pos_1 or K > Pos_2 then
                                  Table(K) = Table'Loop_Entry(K)));

         J := J + 1;
      end loop;
   end Update;

   procedure Add (V : Integer; Max : Positive) is
   begin
      if V < Max then
         Resultado := Resultado + V;
      else
         Resultado := Resultado + 1;
      end if;
   end Add;

   function Copy (Table : T_Table;
                  Min : Positive;
                  Max : Positive;
                  Value : Natural) return T_Table is
      Res : T_Table := Table;
      J : Positive := Min;
   begin
      while J <= Max loop
         Res(J) := Value;

         pragma Loop_Variant (Increases => J);
         pragma Loop_Invariant (J in Min..Max);
         pragma Loop_Invariant (for all K in Min..J =>
                                  Res(K) = Value);
         pragma Loop_Invariant (for all K in Res'Range =>
                                  (if K < Min or K > Max then
                                  Res(K) = Res'Loop_Entry(K)));

         J := J + 1;
      end loop;
      return Res;

   end Copy;

   procedure Resta (A : Integer; B : Integer; R : out Integer) is
   begin
      R := A - B;
   end Resta;

   procedure Doble (Table: in out T_Table) is
      J : Positive := Table'First;
   begin
      while J <= Table'Last loop
         Table(J) := Table(J) * 2;

         pragma Loop_Variant (Increases => J);
         pragma Loop_Invariant (J in Table'Range);
         pragma Loop_Invariant (for all K in Table'First..J =>
                                  Table(K) >= Table'Loop_Entry(K));
         pragma Loop_Invariant (for all K in J+1..Table'Last =>
                               Table(K) = Table'Loop_Entry(K));

         J := J + 1;
      end loop;
   end Doble;

end Example;
