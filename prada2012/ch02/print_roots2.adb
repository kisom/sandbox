-- print_roots2 demonstrates runtime exceptions.
with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Print_Roots is
	use Ada.Text_IO;
	use Ada.Float_Text_IO;
	package Numerics is new Ada.Numerics.Generic_Elementary_Functions(Float);
	use Numerics;
	X: Float;
begin
	Put_Line("Roots of various numbers");
	New_Line(1);

	loop
		Get(X);
		exit when X = 0.0;
		Put("Root of "); Put(X); Put(" is ");
		Put(Sqrt(X));
		New_Line;
	end loop;
end Print_Roots;
