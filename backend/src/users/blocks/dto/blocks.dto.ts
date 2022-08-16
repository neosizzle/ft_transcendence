import { IsNumber, IsOptional, IsString } from "class-validator";

export class BlocksDto {
  @IsNumber()
  @IsOptional()
  id?: number;

  @IsString()
  @IsOptional()
  intraName?: string;
}
