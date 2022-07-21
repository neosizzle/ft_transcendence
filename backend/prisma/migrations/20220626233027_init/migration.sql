-- CreateEnum
CREATE TYPE "UserStatus" AS ENUM ('LOGGEDIN', 'OFFLINE', 'INGAME');

-- CreateTable
CREATE TABLE "User" (
    "id" SERIAL NOT NULL,
    "email" TEXT,
    "avatar" TEXT,
    "status" "UserStatus" NOT NULL DEFAULT E'LOGGEDIN',
    "level" DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    "intraID" TEXT NOT NULL,
    "intraName" TEXT NOT NULL,

    CONSTRAINT "User_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "User_email_key" ON "User"("email");
