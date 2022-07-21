-- CreateEnum
CREATE TYPE "FriendshipStatus" AS ENUM ('PENDING', 'APPROVED', 'REJECTED');

-- CreateTable
CREATE TABLE "Friendship" (
    "id" SERIAL NOT NULL,
    "status" "FriendshipStatus" NOT NULL DEFAULT E'PENDING',
    "userId" INTEGER NOT NULL,
    "friendId" INTEGER NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "Friendship_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "Block" (
    "id" SERIAL NOT NULL,
    "blockerId" INTEGER NOT NULL,
    "blockeeId" INTEGER NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "Block_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "Friendship_userId_key" ON "Friendship"("userId");

-- CreateIndex
CREATE UNIQUE INDEX "Friendship_friendId_key" ON "Friendship"("friendId");

-- CreateIndex
CREATE UNIQUE INDEX "Block_blockerId_key" ON "Block"("blockerId");

-- CreateIndex
CREATE UNIQUE INDEX "Block_blockeeId_key" ON "Block"("blockeeId");

-- AddForeignKey
ALTER TABLE "Friendship" ADD CONSTRAINT "Friendship_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Friendship" ADD CONSTRAINT "Friendship_friendId_fkey" FOREIGN KEY ("friendId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Block" ADD CONSTRAINT "Block_blockerId_fkey" FOREIGN KEY ("blockerId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Block" ADD CONSTRAINT "Block_blockeeId_fkey" FOREIGN KEY ("blockeeId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
