import { IsEmail, IsNotEmpty, IsString } from "class-validator";

export class OtpRequestDto {
  @IsEmail()
  @IsNotEmpty()
  email: string;
}

export class OtpVerifyDto {
  @IsString()
  @IsNotEmpty()
  otp: string;

  @IsString()
  @IsNotEmpty()
  key: string;

  @IsString()
  @IsNotEmpty()
  check: string;
}
