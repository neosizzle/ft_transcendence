import { IsNotEmpty, IsNumber } from "class-validator";

export class AdminDto {
  @IsNumber()
  @IsNotEmpty()
  roomId: number;

  @IsNumber()
  @IsNotEmpty()
  userId: number;
}
