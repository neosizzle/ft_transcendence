import { IsNotEmpty, IsNumber, IsOptional, IsString } from "class-validator";

export class MemberDto {
  @IsNumber()
  @IsNotEmpty()
  roomId: number;

  @IsOptional()
  @IsString()
  password: string;
}
