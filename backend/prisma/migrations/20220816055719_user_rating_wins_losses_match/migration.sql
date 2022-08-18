-- AlterTable
ALTER TABLE "User" ADD COLUMN     "losses" INTEGER NOT NULL DEFAULT 0,
ADD COLUMN     "ranking" INTEGER NOT NULL DEFAULT 1000,
ADD COLUMN     "wins" INTEGER NOT NULL DEFAULT 0;

-- CreateTable
CREATE TABLE "Match" (
    "id" SERIAL NOT NULL,
    "playerId0" INTEGER NOT NULL,
    "playerId1" INTEGER NOT NULL,
    "winnerId" INTEGER NOT NULL,
    "playerScore0" INTEGER NOT NULL,
    "playerScore1" INTEGER NOT NULL,
    "isCustom" BOOLEAN NOT NULL DEFAULT false,

    CONSTRAINT "Match_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "Match" ADD CONSTRAINT "Match_playerId0_fkey" FOREIGN KEY ("playerId0") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Match" ADD CONSTRAINT "Match_playerId1_fkey" FOREIGN KEY ("playerId1") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Match" ADD CONSTRAINT "Match_winnerId_fkey" FOREIGN KEY ("winnerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
