import { IsDateString, IsNotEmpty, IsNumber } from "class-validator";

export class muteDto {
  @IsNotEmpty()
  @IsNumber()
  userId: number;

  @IsNotEmpty()
  @IsNumber()
  roomId: number;

  @IsNotEmpty()
  @IsDateString()
  expiresAt: Date;
}
