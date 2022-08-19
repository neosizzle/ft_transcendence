-- CreateTable
CREATE TABLE "Movie" (
    "id" SERIAL NOT NULL,
    "director" TEXT NOT NULL,
    "movieName" TEXT NOT NULL,
    "yearReleased" INTEGER NOT NULL,

    CONSTRAINT "Movie_pkey" PRIMARY KEY ("id")
);
