/*
  Warnings:

  - Added the required column `token` to the `Auth` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "Auth" ADD COLUMN     "token" TEXT NOT NULL;
