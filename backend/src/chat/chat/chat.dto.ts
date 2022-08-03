import {IsNotEmpty, IsNumber, IsString } from "class-validator";

export class chatDto {
    @IsNotEmpty()
    @IsNumber()
    userId: number;

  @IsNotEmpty()
  @IsNumber()
  roomId: number;

  @IsNotEmpty()
  @IsString()
  message: string;
}
