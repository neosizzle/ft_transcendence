const runFriendsTests = (pactum: any) => {
    describe('Friends', () => {
    
        it(`/friends (POST) bad dto`, () => {
          const dto = {
            "intraName" : "wtf"
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(404)
          .expectBodyContains("error")
        });

        it(`/friends (POST) bad dto 2`, () => {
          const dto = {
            
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
          .expectBodyContains("error")
        });

        it(`/friends (POST) bad dto 3`, () => {
          const dto = {
            "intraName" : "test",
            "id" : "69"
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
          .expectBodyContains("error")
        });

        it(`/friends (POST) bad dto 4`, () => {
          const dto = {
            "id" : "0"
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
          .expectBodyContains("error")
        });

        it(`/friends (POST)`, () => {
          const dto = {
            "intraName" : "test"
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(201)
          .expectBodyContains("PENDING")
        });

        it(`/friends (POST) 2`, () => {
          const dto = {
            "intraName" : "tes"
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(201)
          .expectBodyContains("PENDING")
        });
    
        it(`/friends (POST) 3`, () => {
          const dto = {
            "intraName" : "test2"
          }

          return pactum
          .spec()
          .post(`/friends`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(201)
          .expectBodyContains("PENDING")
        });

        it(`/friends (GET) no pgination`, () => {
          return pactum
          .spec()
          .get(`/friends`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
          .expectBodyContains("error")
        });

        it(`/friends (GET) pagination`, () => {
          return pactum
          .spec()
          .get(`/friends?page=1&pageSize=3`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectBodyContains("tes")
          .expectBodyContains("test")
        });

        it(`/friends (GET) filtering`, () => {
          return pactum
          .spec()
          .get(`/friends?page=1&pageSize=30&filterOn=avatar,status&filterBy=asdf.avatar.jpg,OFFLINE`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 2)
        });

        it(`/friends (GET) filtering 2`, () => {
          return pactum
          .spec()
          .get(`/friends?page=1&pageSize=30&filterOn=reqStatus,reqStatus&filterBy=PENDING,APPROVED`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 3)
          .stores('friendid', 'data[0].friend.id')
        });

        it(`/friends (DELETE) no effect`, () => {
          return pactum
          .spec()
          .delete(`/friends/0`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonMatch({count: 0})
        });

        it(`/friends (DELETE) deleted`, () => {
          return pactum
          .spec()
          .delete(`/friends/$S{friendid}`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonMatch({count: 1})
        });

      })
}

export default runFriendsTests;