import React, { FunctionComponent } from "react";
import { User } from "../../context/authContext";

interface MemberListProps {
  memberUsers: User[] | null;
}

const MemberList: FunctionComponent<MemberListProps> = ({ memberUsers }) => {
  return (
    <div>
      <div className="border-2 text-center text-5xl">Members</div>
      <div>
        {memberUsers?.map((user) => (
          <div
            key={user.id}
            className="border-2 text-center text-xl"
          >{`${user.username}`}</div>
        ))}
      </div>
    </div>
  );
};

export default MemberList;
