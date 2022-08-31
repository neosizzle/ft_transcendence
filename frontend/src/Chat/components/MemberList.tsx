import React, { FunctionComponent } from "react";
import { User } from "../../context/authContext";
import { Member } from "../classes";
import MemberCard from "./MemberCard";

interface MemberListProps {
  memberUsers: User[] | null;
  members : Member[] | null;
}

const MemberList: FunctionComponent<MemberListProps> = ({ memberUsers, members }) => {

  return (
    <div>

      <div className="border-2 text-center text-5xl">Members</div>

      <div>
        {memberUsers?.map((user) => (
          <MemberCard user = {user} key = {user.id}/>
        ))}
      </div>
    </div>
  );
};

export default MemberList;
