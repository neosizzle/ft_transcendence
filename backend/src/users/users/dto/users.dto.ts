import { UserStatus } from "@prisma/client";
import { IsEmail, IsNumber, IsOptional, IsString } from "class-validator";

export class UserPatchDto {
  @IsEmail()
  @IsString()
  @IsOptional()
  email: string;

  @IsString()
  @IsOptional()
  avatar: string;

  @IsString()
  @IsOptional()
  username: string;

  @IsString()
  @IsOptional()
  status: UserStatus;

  @IsOptional()
  @IsNumber()
  level: number;
}
