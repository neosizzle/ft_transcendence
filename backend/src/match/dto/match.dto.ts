import { IsBoolean, IsNumber, IsOptional } from "class-validator";

export class MatchDto {
  @IsNumber()
  playerId0: number;

  @IsNumber()
  playerId1: number;

  @IsNumber()
  playerScore0: number;

  @IsNumber()
  playerScore1: number;

  @IsNumber()
  winnerId: number;

  @IsOptional()
  @IsBoolean()
  isCustom: boolean;
}
