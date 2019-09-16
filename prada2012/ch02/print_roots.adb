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
		if X < 0.0 then
			Put("not calculable.");
		else
			Put(Sqrt(X));
		end if;
		New_Line;
	end loop;
end Print_Roots;
