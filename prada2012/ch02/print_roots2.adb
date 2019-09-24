-- print_roots2 demonstrates runtime exceptions.
with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Print_Roots2 is
	use Ada.Text_IO;
	use Ada.Float_Text_IO;
	use Ada.Numerics;
	package Numerics is new Ada.Numerics.Generic_Elementary_Functions(Float);
	use Numerics;
	X: Float;
begin
	Put_Line("Roots of various numbers");
	New_Line(1);

	loop
		Get(X);
		exit when X = 0.0;
		begin
			Put("Root of "); Put(X); Put(" is ");
		exception
			when Ada.Numerics.Argument_Error =>
				Put("not calculable: X must be >= 0");
			when Constraint_Error =>
				Put("not calculable: X must be >= 0");
		end;
		Put(Sqrt(X));
		New_Line;
	end loop;
end Print_Roots2;
