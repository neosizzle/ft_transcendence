import { IsNegative, IsNumber, IsOptional, IsString, MinLength } from "class-validator";

export class roomPatchDto {
  @IsOptional()
  @IsString()
  roomName: string;

  @IsString()
  @IsOptional()
  @MinLength(7)
  password: string;

  @IsNumber()
  @IsOptional()
  ownerId : number;
}
